{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- What the analysis reports about Core that GHC really produced. Each case is
-- Haskell source, compiled in-process as far as the desugarer.
module Main (main) where

import safe "base" Control.Monad ((=<<), (>>=))
import safe "base" Data.Bool (Bool (False))
import safe "base" Data.Function (($))
import safe "base" Data.List (unlines)
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)))
import safe "base" Data.Maybe (maybe)
import safe "base" Data.String (String)
import safe "base" System.IO (IO)
import "hspec" Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    pendingWith,
    runIO,
    shouldBe,
    xit,
  )
import safe "no-recursion" NoRecursion.Internal
  ( Opts (ignoreMethodCycles, ignoredMethods),
    defaultOpts,
  )
import safe "recursion-analysis" GHC.Recursion (Record (Record))
import "this" NoRecursion.Test.Driver
  ( Analysis (Analysis, coreRecords, renamedRecords),
    Outcome (Analyzed),
    analyze,
    findLibdir,
  )

-- | A `Data.Semigroup.Semigroup` instance whose methods all reach the class
--   dictionary, so that the desugarer gives them a mutually recursive group —
--   the whole reason `ignoreMethodCycles` and `ignoredMethods` exist — with a
--   recursive helper nested inside each.
--
--   Every method is written out rather than left to its default. A default
--   method’s body comes from @base@, so a case that named the binders in one
--   would be asserting on @base@’s implementation of @stimes@ rather than on
--   this analysis, and would break whenever that changed.
semigroupInstance :: String
semigroupInstance =
  unlines
    [ "module M where",
      "import Data.List.NonEmpty (NonEmpty ((:|)))",
      "import Data.Semigroup (Semigroup (sconcat, stimes, (<>)))",
      "data E = A | B",
      "instance Semigroup E where",
      "  A <> A = A",
      "  _ <> _ = B",
      "  stimes _ e = f e",
      "    where",
      "      f x = f (g x)",
      "        where",
      "          g y = g (y <> y)",
      "  sconcat (e :| es) = go e es",
      "    where",
      "      go x [] = x",
      "      go x (y : ys) = x <> go y ys"
    ]

-- | The outcome of an analysis that got as far as Core and found these records
--   there.
inCore :: [Record String] -> Outcome
inCore recs = Analyzed Analysis {renamedRecords = [], coreRecords = recs}

spec :: Spec
spec =
  describe "the analysis over real Haskell" $
    maybe
      ( it "has a GHC to compile the cases with" $
          pendingWith
            "no GHC matching this build was found. Set \
            \NO_RECURSION_TEST_GHC_LIBDIR to point at one."
      )
      ( \libdir -> do
          it "reports a self-recursive function at the top level" $
            analyze
              libdir
              defaultOpts
              ( unlines
                  [ "module M where",
                    "f :: Int -> Int",
                    "f n = if n <= 0 then 0 else f (n - 1)"
                  ]
              )
              >>= (`shouldBe` inCore [Record [] ("f" :| [])])

          it "reports nothing for a function that calls nothing" $
            analyze
              libdir
              defaultOpts
              (unlines ["module M where", "f :: Int -> Int", "f n = n"])
              >>= (`shouldBe` inCore [])

          it "names both binders of mutual recursion" $
            analyze
              libdir
              defaultOpts
              ( unlines
                  [ "module M where",
                    "isEven :: Int -> Bool",
                    "isEven n = if n == 0 then True else isOdd (n - 1)",
                    "isOdd :: Int -> Bool",
                    "isOdd n = if n == 0 then False else isEven (n - 1)"
                  ]
              )
              -- NOTE: The order is the desugarer’s, not the source’s.
              >>= (`shouldBe` inCore [Record [] ("isOdd" :| ["isEven"])])

          -- NOTE: Pending because `allowBind` answers `True` for every
          --       `NonRec`, so @f@ — which does not call itself — is dropped
          --       before the traversal, taking @go@ with it.
          xit "finds recursion in the where clause of a non-recursive function" $
            analyze
              libdir
              defaultOpts
              ( unlines
                  [ "module M where",
                    "f :: Int -> Int",
                    "f n = go n",
                    "  where",
                    "    go k = if k <= 0 then 0 else go (k - 1)"
                  ]
              )
              >>= (`shouldBe` inCore [Record ["f"] ("go" :| [])])

          it "allows everything in a module annotated Recursion" $
            analyze
              libdir
              defaultOpts
              ( unlines
                  [ "module M where",
                    "{-# ANN module \"Recursion\" #-}",
                    "f :: Int -> Int",
                    "f n = if n <= 0 then 0 else f (n - 1)"
                  ]
              )
              >>= (`shouldBe` inCore [])

          it "allows a binder annotated Recursion" $
            analyze
              libdir
              defaultOpts
              ( unlines
                  [ "module M where",
                    "f :: Int -> Int",
                    "f n = if n <= 0 then 0 else f (n - 1)",
                    "{-# ANN f \"Recursion\" #-}"
                  ]
              )
              >>= (`shouldBe` inCore [])

          it "sees every method of an instance as mutually recursive" $
            analyze
              libdir
              defaultOpts {ignoreMethodCycles = False}
              semigroupInstance
              >>= ( `shouldBe`
                      inCore
                        [ Record [] ("$fSemigroupE" :| ["$cstimes", "$csconcat"]),
                          Record ["$cstimes"] ("f" :| []),
                          Record ["$cstimes", "f"] ("g" :| []),
                          Record ["$csconcat"] ("go" :| [])
                        ]
                  )

          it "drops only the top-level group with ignoreMethodCycles" $
            analyze libdir defaultOpts semigroupInstance
              >>= ( `shouldBe`
                      inCore
                        [ Record ["$cstimes"] ("f" :| []),
                          Record ["$cstimes", "f"] ("g" :| []),
                          Record ["$csconcat"] ("go" :| [])
                        ]
                  )

          it "drops what is nested inside the methods named by ignoredMethods" $
            analyze
              libdir
              defaultOpts {ignoredMethods = ["stimes", "sconcat"]}
              semigroupInstance
              >>= (`shouldBe` inCore [])
      )
      =<< runIO findLibdir

-- | The test-suite entry point.
--
-- @since 99999
main :: IO ()
main = hspec $ describe "NoRecursion.Test.Driver" spec
