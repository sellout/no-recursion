{-# OPTIONS_GHC -fplugin NoRecursion #-}

-- | A self-recursive top-level definition, with nothing permitting it. This is
--   the shape every escape hatch is tested against: the hatch is supplied by
--   the driver’s flags rather than written here, so the same module compiled
--   without them is the negative case.
module TopLevelRec (recDef) where

recDef :: a -> b
recDef = recDef
