{-# OPTIONS_GHC -fplugin NoRecursion #-}
-- The driver passes @allowRecursion:false@ on the command line. Options are
-- applied in command-line order, and a file-header pragma comes last, so this
-- must win and the module must compile.
{-# OPTIONS_GHC -fplugin-opt NoRecursion:allowRecursion:true #-}

-- | A module whose recursion is permitted only by the later of two conflicting
--   @allowRecursion@ options.
module OptionPrecedence (recDef) where

recDef :: a -> b
recDef = recDef
