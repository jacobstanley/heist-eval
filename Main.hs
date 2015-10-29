{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Data.Monoid
import qualified Data.ByteString as BS
import Blaze.ByteString.Builder
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Lens
import Heist
import Heist.Internal.Types
import Heist.Compiled as C

-- show
splice :: Splice IO
splice = C.runChildren

main = Main.simple

heistConfig =
  --(set hcNamespace "") $
  --(set hcInterpretedSplices defaultInterpretedSplices) $
  --(set hcLoadTimeSplices defaultLoadTimeSplices) $
  (set hcCompiledSplices ("foo" ## splice)) $
  (set hcTemplateLocations [loadTemplates "."]) $
  emptyHeistConfig

simple :: IO ()
simple = do
  heistState <- either (error . show) id <$>
                  (runEitherT $ initHeist heistConfig)
  builder    <- maybe (error "oops: nothing") fst $
                  renderTemplate heistState "home"
  toByteStringIO BS.putStr builder
