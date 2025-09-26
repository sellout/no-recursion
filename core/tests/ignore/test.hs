{-# LANGUAGE Trustworthy #-}

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Function (const, ($))
import safe "base" System.IO (IO)
import safe qualified "this" Test.AllowRecursion as AllowRecursion
import qualified "this" Test.AnnModule as AnnModule
import qualified "this" Test.AnnName as AnnName
import safe qualified "this" Test.IgnoreDefaultImpls as IgnoreDefaultImpls
import safe qualified "this" Test.UnannName as UnannName

main :: IO ()
main =
  pure
    . AllowRecursion.nonRecDef
    . AnnModule.nonRecDef
    . AnnName.nonRecDef
    . UnannName.nonRecDef
    $ const () IgnoreDefaultImpls.Empty
