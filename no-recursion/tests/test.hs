{-# LANGUAGE Trustworthy #-}

import safe "base" Control.Applicative (Applicative (pure))
import safe "base" Control.Category (Category ((.)))
import safe "base" Data.Function (($))
import safe "base" System.IO (IO)
import qualified "this" Test.Module as Module
import qualified "this" Test.Name as Name

main :: IO ()
main = pure . Module.nonRecDef $ Name.nonRecDef ()
