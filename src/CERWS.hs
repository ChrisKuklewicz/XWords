-- | This is continuation style rewrite of a (ContT ErrorT ReaderT
-- WriterT StateT).  The continutation style ensures that there is no
-- quadratic penalty for left-associative use of (>>=) or (>>).
--
-- Currently the ErrorT is outside the R/W/S, but it could be 
module CERWS(CERWS(CERWS,unCERWS),runCERWS
            ,CERWST(CERWST,unCERWST),runCERWST
            ,ERWS,ERWST
            ,getCC,getCC1) where

import Data.Monoid(Monoid(mempty,mappend))
import Control.Monad.Fix(fix)
import Control.Monad(MonadPlus(mzero,mplus))
import Control.Monad.Trans(MonadTrans(lift),MonadIO(liftIO))
import Control.Monad.Cont.Class(MonadCont(callCC))
import Control.Monad.Error.Class(MonadError(throwError,catchError),Error(strMsg,noMsg))
import Control.Monad.Reader.Class(MonadReader(ask,local))
import Control.Monad.Writer.Class(MonadWriter(tell,listen,pass))
import Control.Monad.State.Class(MonadState(get,put))

type ERWS e r w s a = forall b. CERWS b e r w s a
type ERWST e r w s m a = forall b. CERWST b e r w s m a

newtype CERWS b e r w s a = CERWS {
  unCERWS :: (a -> r -> w -> s -> b) -- success continuation
          -> (e -> b)                -- error continuation
          -> r                       -- reader
          -> w                       -- log so far
          -> s                       -- state
          -> b                       -- result
  }

newtype CERWST b e r w s m a = CERWST {
  unCERWST :: (a -> r -> w -> s -> m b) -- success continuation
           -> (e -> m b)                -- error continuation
           -> r                         -- reader
           -> w                         -- log so far
           -> s                         -- state
           -> m b                       -- operation
  }

-- CERWS

-- | The runCERWS is the only place where 'Left', 'Right', and (,,)
-- data constructors are actually used.  This is in contrast to
-- (Either e) or (ErrorT e) which construct and consume Left or Right
-- at every (>>=) or (>>).
runCERWS :: (Monoid w) =>
            r -> s -> CERWS (Either e (a,w,s)) e r w s a
         -> Either e (a,w,s)
runCERWS r s1 m = unCERWS m c h r mempty s1
  where c a _ w s2 = Right (a,w,s2)
        h e = Left e

instance (Error e) => Functor (CERWS b e r w s) where
--fmap f m = m >>= return . f
  fmap f m = CERWS (\c -> unCERWS m (c . f))

instance (Error e) => Monad (CERWS b e r w s) where
  return a = CERWS (\c _ -> c a)
  m >>= k  = CERWS (\c e -> unCERWS m (\a -> unCERWS (k a) c e) e)
  fail msg = CERWS (\_ e _ _ _ -> e (strMsg msg))

instance (Error e) => MonadCont (CERWS b e r w s) where
  callCC = callCC_CERWS

-- The continuation does not use its error handler so 'e1' is free.
callCC_CERWS :: ((a -> CERWS b e1 r w s a1) -> CERWS b e r w s a)
             -> CERWS b e r w s a
callCC_CERWS f = CERWS $ \c -> let k a = CERWS (\_ _ -> c a)
                               in unCERWS (f k) c

instance (Error e) => MonadError e (CERWS b e r w s) where
  throwError msg = CERWS (\_ e _ _ _ -> e msg)
  catchError = catchError_CERWS

-- The subcomputation can have a different error type
catchError_CERWS :: CERWS b e1 r w s a
                 -> (e1 -> CERWS b e r w s a)
                 -> CERWS b e r w s a
catchError_CERWS m h = CERWS $ \c e r w s ->
  let h' e' = unCERWS (h e') c e r w s
  in unCERWS m c h' r w s

instance (Error e, Monoid w) => MonadWriter w (CERWS b e r w s) where
  tell w'  = CERWS (\c _ r w -> c () r (mappend w w'))
  listen m = CERWS (\c e r w -> unCERWS m (\a r' w'-> c (a,w') r' (mappend w w')) e r mempty)
  pass     = pass_CERWS

-- What not transform the type as well as value?
pass_CERWS :: (Monoid w1,Monoid w2) => CERWS b e r w1 s (a, w1 -> w2) -> CERWS b e r w2 s a
pass_CERWS m = CERWS (\c e r w -> unCERWS m (\(a,f) r' w'-> c  a r' (mappend w (f w'))) e r mempty)

instance (Error e) => MonadReader r (CERWS b e r w s) where
  ask = CERWS (\c _ r -> c r r)
  local = local_CERWS

-- Why not change the type as well as value?
local_CERWS :: (r1 -> r2) -> CERWS b e r2 w s a -> CERWS b e r1 w s a
local_CERWS f m = CERWS (\c e r -> let c' a _ = c a r
                                   in unCERWS m c' e (f r))
              
instance (Error e) => MonadState s (CERWS b e r w s) where
  get   = CERWS (\c _ r w s -> c s  r w s)
  put s = CERWS (\c _ r w _ -> c () r w s)

instance (Error e) => MonadPlus (CERWS b e r w s) where
  mzero = throwError noMsg
  mplus m1 m2 = catchError m1 (const m2)

-- CERWST

runCERWST :: (Monad m,Monoid w) =>
            r -> s -> CERWST (Either e (a,w,s)) e r w s m a
         -> m (Either e (a,w,s))
runCERWST r s1 m = unCERWST m c h r mempty s1
  where c a _ w s2 = return (Right (a,w,s2))
        h e = return (Left e)

instance (Monad m,Error e) => Functor (CERWST b e r w s m) where
--fmap f m = m >>= return . f
  fmap f m = CERWST (\c -> unCERWST m (c . f))

instance (Monad m,Error e) => Monad (CERWST b e r w s m) where
  return a = CERWST (\c _ -> c a)
  m >>= k  = CERWST (\c e -> unCERWST m (\a -> unCERWST (k a) c e) e)
  fail msg = CERWST (\_ e _ _ _ -> e (strMsg msg))

instance MonadTrans (CERWST b e r w s) where
  lift m = CERWST (\c _ r w s -> m >>= \a -> c a r w s)

instance (MonadIO m,Error e) => MonadIO (CERWST b e r w s m) where
  liftIO = lift . liftIO

instance (Monad m, Error e) => MonadCont (CERWST b e r w s m) where
  callCC = callCC_CERWST

-- The continuation itself never uses its error handler, so 'e1' is free.
callCC_CERWST :: ((a -> CERWST b e1 r w s m a1) -> CERWST b e r w s m a)
             -> CERWST b e r w s m a
callCC_CERWST f = CERWST $ \c -> let k a = CERWST (\_ _ -> c a)
                                 in unCERWST (f k) c

instance (Monad m,Error e) => MonadError e (CERWST b e r w s m) where
  throwError msg = CERWST (\_ e _ _ _ -> e msg)
  catchError = catchError_CERWST

catchError_CERWST :: CERWST b e1 r w s m a
                 -> (e1 -> CERWST b e r w s m a)
                 -> CERWST b e r w s m a
catchError_CERWST m h = CERWST $ \c e r w s ->
  let h' e' = unCERWST (h e') c e r w s
  in unCERWST m c h' r w s

instance (Monad m, Error e) => MonadReader r (CERWST b e r w s m) where
  ask = CERWST (\c _ r -> c r r)
  local = local_CERWST

-- What not change the type as well?
local_CERWST :: (r1 -> r2) -> CERWST b e r2 w s m a -> CERWST b e r1 w s m a
local_CERWST f m = CERWST (\c e r -> let c' a _ = c a r
                                     in unCERWST m c' e (f r))

instance (Monad m, Error e, Monoid w) => MonadWriter w (CERWST b e r w s m) where
  tell w'  = CERWST (\c _ r w -> c () r (mappend w w'))
  listen m = CERWST (\c e r w -> unCERWST m (\a r' w'-> c (a,w') r' (mappend w w')) e r mempty)
  pass     = pass_CERWST

pass_CERWST :: (Monoid w1,Monoid w2) => CERWST b e r w1 s m (a, w1 -> w2) -> CERWST b e r w2 s m a
pass_CERWST m = CERWST (\c e r w -> unCERWST m (\(a,f) r' w'-> c a r' (mappend w (f w'))) e r mempty)

instance (Monad m,Error e) => MonadState s (CERWST b e r w s m) where
  get   = CERWST (\c _ r w s -> c s  r w s)
  put s = CERWST (\c _ r w _ -> c () r w s)

instance (Monad m, Error e) => MonadPlus (CERWST b e r w s m) where
  mzero = throwError noMsg
  mplus m1 m2 = catchError m1 (const m2)

-- getCC and getCC1

getCC :: (MonadCont m) => m (m a)
getCC = callCC (return . fix)

getCC1 :: (MonadCont m) => a -> m (  a->m b  ,  a  )
getCC1 x = callCC $ \k -> let jump a = k (jump,a) in return (jump,x)

{- -- some lingering testing code

testGet :: IO (Either String ((),(),Int))
testGet = runCERWST () (10::Int) $ do
  again <- getCC
  s <- get
  if s <= 0 then throwError "Boom"
            else print ("Countdown from "++show s)
  (jump,val) <- getCC1 s
  if val > 0 then print val >> jump (pred val) 
             else get >>= put . (`div` 2) >> again
  return ()
 where print :: (MonadIO m, Show a) => a -> m ()
       print = liftIO . Prelude.print

baz :: IO (Either String (String,String,String))
baz = runCERWST () "start" $ do
  print "alpha"
  tell "1"
  print =<< callCC (\_ -> return (15::Int))
  put . (++"!") =<< get
  r <- callCC $ \k -> do
         tell "2"
         print "beta"
         put .  (++"@") =<< get
         k (17 :: Integer)
         print "DOES NOT PRINT"
         return 1
  print ("r  is "++show (r))
  return "EOF"
 where print :: (MonadIO m, Show a) => a -> m ()
       print = liftIO . Prelude.print

test :: IO Bool
test = do x <- runCERWST () "Hello" bar
          print x
          return (x==ans)
  where ans = Right ("Hello was gotten","ABc","write value from listen: a")

type T m = ERWST String () String String m String

bar :: T IO
bar = do
  print "alpha"
  (a,w) <- listen (tell "a" >> print "beta" >> get)
  put ("write value from listen: "++w)
  pass $ catchError (tell "cheese" >> print "delta" >> put "wisconsin" >> throwError "b")
                    (\msg -> tell msg >> print "epsilon" >> return ( (), map (toEnum . (subtract 32) . fromEnum)))
  tell "c"
  print "gamma"
  return (a++" was gotten")
 where print = liftIO . Prelude.print

-}

