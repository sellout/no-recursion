{-# LANGUAGE Trustworthy #-}

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Function (const, ($))
import safe "base" System.IO (IO)
import safe "this" Test.AllowRecursion qualified as AllowRecursion
import "this" Test.AnnModule qualified as AnnModule
import "this" Test.AnnName qualified as AnnName
import safe "this" Test.IgnoreDefaultImpls qualified as IgnoreDefaultImpls
import safe "this" Test.UnannName qualified as UnannName

main :: IO ()
main =
  pure
    . AllowRecursion.nonRecDef
    . AnnModule.nonRecDef
    . AnnName.nonRecDef
    . UnannName.nonRecDef
    $ const () IgnoreDefaultImpls.Empty
