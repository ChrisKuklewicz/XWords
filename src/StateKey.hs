-- TODO: add code to detect and expire the older client states.
-- probably by lanching a background thread to periodically sweep
-- through all the stored state.
-- 
-- To do it more efficiently: keep a list or Sequence of the access
-- times of live sessions in sorted order and have the map hold weak
-- values.

module StateKey(IdCookie,SessionKey,GetClient,Client(created,state),History(..),HIST
               ,newSessions) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E
import Control.Monad.Error
import Control.Monad.Trans
import Data.Bits(Bits((.|.),shiftL))
import Data.Ix(inRange)
import Data.List(foldl1')
import Data.Map (Map)
import Data.Time.Clock
import Network.CGI
import Network.Socket
import Numeric
import System.Random
import Text.XHtml.Transitional(Html)
import qualified Data.Map as M

type IdCookie = Integer
idCookie :: String
idCookie = "idCookie"
idBounds :: (IdCookie,IdCookie)
idBounds = (0,pred (2^idCookieBits))
idCookieBits :: Int
idCookieBits = 64

newtype SessionKey = SessionKey (Integer,IdCookie) deriving (Eq,Ord,Show)

data StateMap a = StateKey { jar :: TVar [IdCookie]
                           , store :: TVar (Map SessionKey a)
                           }

data Client b = Client { created :: UTCTime
                       , accessed :: TVar UTCTime
                       , state :: b
                       }

type HIST = (String,Html)

data History = History { history :: TVar [HIST] }

type Sessions = StateMap (Client History)

forkVacuum :: Sessions -> IO ThreadId
forkVacuum (StateKey {store = s}) = forkIO . forever $ do
  threadDelay timeout
  limit <- fmap (addUTCTime ((-15)*60)) getCurrentTime
  m <- atomically (readTVar s)
  let clean (key,Client {accessed = a}) = atomically $ do
        time <- readTVar a
        when (time < limit) $ do
          m' <- readTVar s
          writeTVar s $! M.delete key m'
  mapM_ clean (M.assocs m)
 where timeout = 15*60*(10^(6::Int)) -- 15 minute timeout
       forever x = x >> forever x

-- | Call to create an empty global session cache.  Usually once per program.
-- Needs to be MonadIO to initialize the randomly generated id cookies.
newSessions :: (MonadIO m) => m GetClient
newSessions = liftIO $ do
  s <- liftM2 StateKey (newTVarIO =<< (fmap (map i2w . randomRs idBounds) newStdGen))
                       (newTVarIO (M.empty))
  forkVacuum s
  return (getClient s)
 where i2w :: Integer -> IdCookie
       i2w = fromIntegral

type GetClient = CGI (Either String (SessionKey,Client History))
type GCE a = ErrorT String (CGIT IO) a
stmGCE :: STM (GCE a) -> GCE a
stmGCE = join . liftIO . atomically

-- | Call with global session cache to create or lookup SessionKey and Client data.
-- This works by using the client's IP address and a browser cookie (a random unique id).
getClient :: Sessions -> CGI (Either String (SessionKey,Client History))
getClient (StateKey {jar=jv,store=mv}) = runErrorT foo where
 foo :: GCE (SessionKey,Client History)
 foo = do
  remote <- lift remoteAddr
  addr   <- maybe (throwError ("Could not parse value "++show remote++" of remote ip address.") ) return =<< parseInetAddr' remote
  mId    <- lift (getCookie idCookie) -- mId is untrusted if it is set
  now    <- liftIO getCurrentTime
  let newClientRecord :: STM (GCE (SessionKey,Client History))
      newClientRecord = do
        (newId:restIds) <- readTVar jv
        writeTVar jv restIds
        let newSessionKey = SessionKey (addr,newId)
        client <- newClient now
        m <- readTVar mv
        writeTVar mv $! M.insert newSessionKey client m
        let bar :: GCE (SessionKey, Client History)
            bar = do
              lift (setCookie (newCookie idCookie (show newId)))  -- perhaps overwrites old cookie
              return (newSessionKey,client)
        return bar
  case fmap parseIdCookie mId of
    Nothing            -> stmGCE $ newClientRecord  -- no existing cookie returned by browser
    Just Nothing       -> throwError ("Could not parse value of cookie named "++show idCookie++".")
    Just (Just someId) -> do
      let someSessionKey = SessionKey (addr,someId)
      stmGCE $ do
        mClient <- return . M.lookup someSessionKey =<< readTVar mv
        case mClient of
          Nothing -> newClientRecord
          Just someClient -> do
            updateClient someClient now
            let baz :: GCE (SessionKey, Client History)
                baz = return (someSessionKey,someClient)
            return baz

   where 
       -- The cookie is required to hold precisely a decimal value in the generated range
       parseIdCookie :: String -> Maybe IdCookie
       parseIdCookie str = case readDec str of
                             [(idc,[])] | inRange idBounds idc -> Just idc
                             _ -> Nothing

       -- Smart constructor for (Client History)
       newClient :: UTCTime -> STM (Client History)
       newClient now = liftM2 (Client now) (newTVar now) (fmap History (newTVar []))

       updateClient ::  Client History -> UTCTime -> STM ()
       updateClient client now = writeTVar (accessed client) now

       parseInetAddr' :: (MonadIO m) => String -> m (Maybe Integer)
       parseInetAddr' s = do
         eais <- liftIO . try $ getAddrInfo (Just (defaultHints { addrFlags = [AI_NUMERICHOST]})) (Just s) Nothing
         case eais of
           Right (ai:_) ->
             case addrAddress ai of
               SockAddrInet _portNumber hostAddress ->
                 return (Just (toInteger hostAddress))
               SockAddrInet6 _portNumber _flowInfo (w1,w2,w3,w4) _scopeIO ->
                 return . Just . negate . foldl1' (\i w -> (shiftL i 32) .|. w) . map toInteger $ [w4,w3,w2,w1]
               a -> liftIO (throwError . strMsg $ "StateKey: Unexpected address returned: "++show (s,a))
           Left (err :: IOError) -> liftIO (throwError . strMsg $ "StateKey: No address returned: "++show s++"\n error was: "++show err)

-- The return type of inet_addr does not include the parse failures, which must be caught.
-- But this could not handle the IP6 "::1" string given by Safari instead of localhost.
-- parseInetAddr :: (MonadIO m) => String -> m (Maybe HostAddress)
-- parseInetAddr = liftIO . block . handleJust ioErrors (\_ -> return Nothing) . fmap Just . inet_addr
