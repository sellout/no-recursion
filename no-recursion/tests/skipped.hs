-- __NB__: This module is /not/ @Unsafe@, but because of how it’s conditionally
--         compiled with doctests.hs, Hlint and Ormolu get confused if it’s
--         marked @Safe@.
{-# LANGUAGE Unsafe #-}

-- | This module allow us to skip test suites entirely for certain system
--   configurations.
module Main (main) where

import System.IO (IO, putStrLn)

main :: IO ()
main = putStrLn "Skipped doctests suite on this system."
