{-# LANGUAGE Trustworthy #-}

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Function (($))
import safe "base" System.IO (IO)
import qualified "this" Test.AllowRecursion as AllowRecursion
import qualified "this" Test.AnnModule as AnnModule
import qualified "this" Test.AnnName as AnnName

main :: IO ()
main =
  pure . AllowRecursion.nonRecDef . AnnModule.nonRecDef $ AnnName.nonRecDef ()
