{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Main (main) where

import safe "base" Data.Function (($))
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)))
import safe "base" System.IO (IO)
import "ghc" GHC.Plugins qualified as Plugins
import "hspec" Test.Hspec (Spec, describe, hspec, it, shouldBe)
import safe "recursion-analysis" GHC.Recursion qualified as Recursion
import "this" GHC.Recursion.Test.Quote (core)

spec :: Spec
spec =
  describe "inBind" do
    it "reports a rec group at the top level" $
      Recursion.inBind (Plugins.Rec [("f", [core| _ |])])
        `shouldBe` [Recursion.Record [] ("f" :| [])]

    it "names every binder of a mutually recursive group" $
      Recursion.inBind (Plugins.Rec [("f", [core| _ |]), ("g", [core| _ |])])
        `shouldBe` [Recursion.Record [] ("f" :| ["g"])]

    it "reports nothing for a binding with no recursion under it" $
      Recursion.inBind (Plugins.NonRec "f" [core| _ |]) `shouldBe` []

    -- The traversal reports this. `failOnRecursion` is what decides whether
    -- anyone hears about it, and that is tested on the plugin side.
    it "finds recursion inside a non-recursive binding" $
      Recursion.inBind
        (Plugins.NonRec "f" [core| \x -> letrec { go = _ } in _ |])
        `shouldBe` [Recursion.Record ["f"] ("go" :| [])]

    it "reports recursion under a rec group beneath its binder" $
      Recursion.inBind
        ( Plugins.Rec
            [ ( "f",
                [core| \x -> case _ of { _ -> _; _ -> letrec { go = _ } in _ } |]
              )
            ]
        )
        `shouldBe` [ Recursion.Record [] ("f" :| []),
                     Recursion.Record ["f"] ("go" :| [])
                   ]

    it "finds recursion under a cast" $
      Recursion.inBind
        (Plugins.Rec [("f", [core| cast (letrec { go = _ } in _) |])])
        `shouldBe` [ Recursion.Record [] ("f" :| []),
                     Recursion.Record ["f"] ("go" :| [])
                   ]

    it "looks at both sides of an application" $
      Recursion.inBind
        ( Plugins.Rec
            [ ( "f",
                [core| (letrec { go = _ } in _) (letrec { stop = _ } in _) |]
              )
            ]
        )
        `shouldBe` [ Recursion.Record [] ("f" :| []),
                     Recursion.Record ["f"] ("go" :| []),
                     Recursion.Record ["f"] ("stop" :| [])
                   ]

-- | The test-suite entry point.
--
-- @since 99999
main :: IO ()
main = hspec $ describe "GHC.Recursion" spec
