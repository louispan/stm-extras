module Control.Concurrent.STM.TMVar.Extras where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

-- | forces a swap of TMVar irreguardless if it previously contained a value or not.
-- Returns what was in the TMVar (if exists)
forceSwapTMVar :: TMVar a -> a -> STM (Maybe a)
forceSwapTMVar v a = (Just <$> swapTMVar v a) `orElse` (const Nothing <$> putTMVar v a)
