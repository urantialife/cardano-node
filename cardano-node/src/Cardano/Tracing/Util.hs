{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Tracing.Util
  ( fragmentLength
  ) where

import           Prelude
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), HasHeader (..),
                   blockNo)
import           Ouroboros.Network.Point (fromWithOrigin)

fragmentLength :: HasHeader header => AF.AnchoredFragment header -> Int
fragmentLength af = fromIntegral $ blockN - firstBlock
  where
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo af)
    firstBlock = case unBlockNo . blockNo <$> AF.last af of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b
