{-# LANGUAGE Safe #-}

import "base" Control.Applicative (pure)
import "base" Data.Function (const, ($))
import "base" System.IO (IO)
import qualified "this" Test.IgnoreDefaultImpls as IgnoreDefaultImpls

main :: IO ()
main = pure $ const () IgnoreDefaultImpls.Empty
