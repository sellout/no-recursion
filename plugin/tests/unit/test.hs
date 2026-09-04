{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- What the plugin decides to report, asserted as values. The traversal itself is
-- tested in `recursion-analysis`; what is under test here is the policy laid
-- over it — the options, the annotations, and the rules about what to overlook.
--
-- Nothing here starts a compiler session: the binder is `String`, so rendering
-- one is `Data.Function.id` and its annotations come from a list.
module Main (main) where

import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Either (either)
import safe "base" Data.Foldable (toList)
import safe "base" Data.Function (const, id, ($))
import safe "base" Data.List (lookup)
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)))
import safe "base" Data.Maybe (fromMaybe)
import safe "base" Data.String (String)
import safe "base" System.IO (IO)
import "ghc" GHC.Plugins qualified as Plugins
import "hspec" Test.Hspec (Spec, describe, hspec, it, shouldBe, xit)
import safe "no-recursion" NoRecursion.Internal
  ( Opts (allowRecursion, ignoreMethodCycles, ignoredDecls, ignoredMethods),
    defaultOpts,
    failOnRecursion,
  )
import safe "recursion-analysis" GHC.Recursion (Record (Record))

-- | An expression the traversal walks straight past. What is under test is
--   never the leaf.
leaf :: Plugins.Expr String
leaf = Plugins.Lit (Plugins.mkLitInt64 0)

-- | A recursive group bound under @v@, which is what most of these need.
recursiveUnder :: String -> String -> Plugins.Bind String
recursiveUnder v go =
  Plugins.NonRec v . Plugins.Let (Plugins.Rec [(go, leaf)]) $ leaf

-- | What the plugin reports about some Core, given the options, the module’s
--   annotations and each binder’s.
analyzed ::
  Opts ->
  [String] ->
  [(String, [String])] ->
  [Plugins.Bind String] ->
  [Record String]
analyzed opts modAnns anns =
  either toList (const [])
    . failOnRecursion id (\v -> fromMaybe [] $ lookup v anns) modAnns opts

spec :: Spec
spec =
  describe "failOnRecursion" do
    it "reports a top-level rec group" $
      analyzed defaultOpts [] [] [Plugins.Rec [("f", leaf)]]
        `shouldBe` [Record [] ("f" :| [])]

    -- Pending: the traversal finds the `go` below, but `allowBind` answers
    -- `True` for every `NonRec`, so it is dropped before anyone is told. The
    -- branch that looks inside every top-level binding is what makes this run.
    xit "reports recursion inside a top-level non-recursive binding" $
      analyzed defaultOpts [] [] [recursiveUnder "f" "go"]
        `shouldBe` [Record ["f"] ("go" :| [])]

    it "lets a top-level rec group through with allowRecursion" $
      analyzed
        defaultOpts {allowRecursion = True}
        []
        []
        [Plugins.Rec [("f", leaf)]]
        `shouldBe` []

    it "prefers a NoRecursion annotation on a binder to allowRecursion" $
      analyzed
        defaultOpts {allowRecursion = True}
        []
        [("f", ["NoRecursion"])]
        [Plugins.Rec [("f", leaf)]]
        `shouldBe` [Record [] ("f" :| [])]

    it "allows a binder annotated Recursion without the option" $
      analyzed defaultOpts [] [("f", ["Recursion"])] [Plugins.Rec [("f", leaf)]]
        `shouldBe` []

    it "prefers a NoRecursion module annotation to a Recursion one" $
      analyzed
        defaultOpts
        ["Recursion", "NoRecursion"]
        []
        [Plugins.Rec [("f", leaf)]]
        `shouldBe` [Record [] ("f" :| [])]

    it "drops a group named by ignoredDecls" $
      analyzed
        defaultOpts {ignoredDecls = ["f"]}
        []
        []
        [Plugins.Rec [("f", leaf)]]
        `shouldBe` []

    it "drops a top-level group of desugarer names with ignoreMethodCycles" $
      analyzed defaultOpts [] [] [Plugins.Rec [("$cfoo", leaf)]] `shouldBe` []

    it "reports that same group without ignoreMethodCycles" $
      analyzed
        defaultOpts {ignoreMethodCycles = False}
        []
        []
        [Plugins.Rec [("$cfoo", leaf)]]
        `shouldBe` [Record [] ("$cfoo" :| [])]

    it "drops what is nested under a method named by ignoredMethods" $
      analyzed
        defaultOpts {ignoreMethodCycles = False, ignoredMethods = ["foo"]}
        []
        []
        [Plugins.Rec [("$cfoo", Plugins.Let (Plugins.Rec [("go", leaf)]) leaf)]]
        `shouldBe` [Record [] ("$cfoo" :| [])]

-- | The test-suite entry point.
--
-- @since 99999
main :: IO ()
main = hspec $ describe "NoRecursion.Internal" spec
