{-# OPTIONS_GHC -fplugin NoRecursion #-}

-- | The probe fixture: it enables the plugin but contains no recursion, so it
--   must compile. If it doesn’t, the test environment can’t load the plugin and
--   no verdict from the other fixtures means anything.
module Ok (double) where

double :: Int -> Int
double n = n + n
