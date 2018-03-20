module Control.Concurrent.STM.TMVar.Extras where

import Control.Concurrent.STM

-- | Non-blocking swap of TMVar irreguardless if it previously contained a value or not.
-- Returns what was in the TMVar (if exists)
forceSwapTMVar :: TMVar a -> a -> STM (Maybe a)
forceSwapTMVar v a = (Just <$> swapTMVar v a) `orElse` (const Nothing <$> putTMVar v a)

-- | Block until TMVar is empty.
-- The argument a is used to try to put into TMVar, but is taken out again in the same
-- transaction
waitTillEmptyTMVar :: TMVar a -> a -> STM ()
waitTillEmptyTMVar v a = putTMVar v a >> takeTMVar v >> pure ()

-- | Block until TMVar is full.
waitTillFullTMVar :: TMVar a -> STM ()
waitTillFullTMVar v = takeTMVar v >>= putTMVar v

-- | Similar to 'Control.Concurrent.MVar.modifyMVar'.
-- A slight variation on 'modifyTMVar_' that allows a value to be returned
-- (b) in addition to the modified value of the TMVar.
modifyTMVar :: TMVar a -> (a -> STM (a, b)) -> STM b
modifyTMVar v k = do
    a <- takeTMVar v
    (a', b) <- k a
    putTMVar v a'
    pure b

maybeModifyTMVar :: TMVar a -> (a -> STM (Maybe a, b)) -> STM b
maybeModifyTMVar v k = do
    a <- takeTMVar v
    (a', b) <- k a
    maybe (pure ()) (putTMVar v) a'
    pure b

-- | Similar to 'Control.Concurrent.MVar.modifyMVar_'.
-- Modify the contents of a TMVar.
modifyTMVar_ :: TMVar a -> (a -> STM a) -> STM ()
modifyTMVar_ v k = do
    a <- takeTMVar v
    a' <- k a
    putTMVar v a'

maybeModifyTMVar_ :: TMVar a -> (a -> STM (Maybe a)) -> STM ()
maybeModifyTMVar_ v k = do
    a <- takeTMVar v
    a' <- k a
    maybe (pure ()) (putTMVar v) a'
