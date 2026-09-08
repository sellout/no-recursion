-- | The same shape as @TopLevelRec@, but with no @OPTIONS_GHC@ pragma of its
--   own. The driver supplies @-fplugin NoRecursion@ on the command line
--   instead, which is the path a @ghc-options@ field in a Cabal file takes.
module PlainRec (recDef) where

recDef :: a -> b
recDef = recDef
