{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- A quasi-quoter for Core expressions.
--
-- The grammar is deliberately tiny, and deliberately not Haskell — what’s
-- under test is the shape GHC hands the analysis, so the case should say that
-- shape and nothing else.
--
-- > expr  := '\' var '->' expr
-- >        | 'case' expr 'of' '{' alts '}'
-- >        | 'let' bind 'in' expr
-- >        | 'letrec' '{' binds '}' 'in' expr
-- >        | 'cast' atom
-- >        | atom+
-- > atom  := '_' | '(' expr ')'
-- > alts  := alt (';' alt)*
-- > alt   := '_' '->' expr
-- > binds := bind (';' bind)*
-- > bind  := var '=' expr
--
-- Braces are required rather than inferred.
--
-- @_@ is the only leaf, and there is no way to write a variable /occurrence/.
-- The plugin ignores occurrences entirely. If that changes, the quoter would
-- have to produce @b ~ `Plugins.CoreBndr`@ using `Plugins.mkSysLocal` over a
-- `Plugins.UniqSupply`, and the cases would compare after @`fmap`
-- `Plugins.getOccString`@.
module GHC.Recursion.Test.Quote (core) where

import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (fail, (>>=))
import safe "base" Data.Bifunctor (first)
import safe "base" Data.Bool (otherwise, (||))
import safe "base" Data.Char (isAlpha, isAlphaNum, isSpace)
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Eq (Eq ((==)))
import safe "base" Data.Foldable (elem, foldl', notElem)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.List (span)
import safe "base" Data.Maybe (Maybe (Just))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst, snd, uncurry)
import safe "base" Text.Show (Show (show))
import "ghc" GHC.Plugins qualified as Plugins
import "template-haskell" Language.Haskell.TH qualified as TH
import "template-haskell" Language.Haskell.TH.Quote
  ( QuasiQuoter
      ( QuasiQuoter,
        quoteDec,
        quoteExp,
        quotePat,
        quoteType
      ),
  )

-- | Quotes a Core expression, producing an @`Plugins.Expr` `String`@.
--
-- > [core| \f -> case _ of { _ -> _; _ -> letrec { go = _ } in _ } |]
--
-- @since 99999
core :: QuasiQuoter
core =
  QuasiQuoter
    { quoteExp = either fail (pure . toExp) . parseCore,
      quotePat = \_ -> fail "[core| … |] only quotes expressions",
      quoteType = \_ -> fail "[core| … |] only quotes expressions",
      quoteDec = \_ -> fail "[core| … |] only quotes expressions"
    }

type Token :: Type
data Token
  = Lambda
  | Arrow
  | Equals
  | Open
  | Close
  | BraceOpen
  | BraceClose
  | Semi
  | Hole
  | Ident String
  deriving stock (Eq, Show)

-- | What the grammar above parses to, before it becomes a `TH.Exp`.
type Syntax :: Type
data Syntax
  = Lam String Syntax
  | App Syntax Syntax
  | Let String Syntax Syntax
  | Letrec [(String, Syntax)] Syntax
  | Case Syntax [Syntax]
  | Cast Syntax
  | Leaf

keywords :: [String]
keywords = ["case", "cast", "in", "let", "letrec", "of"]

tokenise :: String -> Either String [Token]
tokenise = \case
  [] -> pure []
  '-' : '>' : cs -> (Arrow :) <$> tokenise cs
  '\\' : cs -> (Lambda :) <$> tokenise cs
  '=' : cs -> (Equals :) <$> tokenise cs
  '(' : cs -> (Open :) <$> tokenise cs
  ')' : cs -> (Close :) <$> tokenise cs
  '{' : cs -> (BraceOpen :) <$> tokenise cs
  '}' : cs -> (BraceClose :) <$> tokenise cs
  ';' : cs -> (Semi :) <$> tokenise cs
  '_' : cs -> (Hole :) <$> tokenise cs
  all'@(c : cs)
    | isSpace c -> tokenise cs
    | isAlpha c ->
        let named = span (\x -> isAlphaNum x || x == '\'') all'
         in (Ident (fst named) :) <$> tokenise (snd named)
    | otherwise -> Left $ "unexpected character " <> show c <> " in " <> cs

parseCore :: String -> Either String Syntax
parseCore src = do
  toks <- tokenise src
  parsed <- expr toks
  case snd parsed of
    [] -> pure $ fst parsed
    rest -> Left $ "unconsumed input: " <> show rest

expr :: [Token] -> Either String (Syntax, [Token])
expr = \case
  Lambda : Ident v : Arrow : ts | v `notElem` keywords -> first (Lam v) <$> expr ts
  Ident "case" : ts -> do
    scrut <- expr ts
    case snd scrut of
      Ident "of" : BraceOpen : ts' -> do
        alts <- sepBy alt ts'
        case snd alts of
          BraceClose : ts'' -> pure (Case (fst scrut) (fst alts), ts'')
          rest -> Left $ "expected ‘}’ closing a case, found " <> show rest
      rest -> Left $ "expected ‘of {’ after a case scrutinee, found " <> show rest
  Ident "let" : ts -> do
    bnd <- bind ts
    case snd bnd of
      Ident "in" : ts' ->
        first (uncurry Let (fst bnd)) <$> expr ts'
      rest -> Left $ "expected ‘in’ after a let binding, found " <> show rest
  Ident "letrec" : BraceOpen : ts -> do
    bnds <- sepBy bind ts
    case snd bnds of
      BraceClose : Ident "in" : ts' -> first (Letrec (fst bnds)) <$> expr ts'
      rest -> Left $ "expected ‘} in’ after letrec bindings, found " <> show rest
  Ident "cast" : ts -> first Cast <$> atom ts
  ts -> application ts

-- | One or more atoms, applied left to right.
application :: [Token] -> Either String (Syntax, [Token])
application ts = atom ts >>= more
  where
    more acc = case snd acc of
      rest@(t : _)
        | t `elem` [Hole, Open] -> do
            next <- atom rest
            more (first (App (fst acc)) next)
      _ -> pure acc

atom :: [Token] -> Either String (Syntax, [Token])
atom = \case
  Hole : ts -> pure (Leaf, ts)
  Open : ts -> do
    inner <- expr ts
    case snd inner of
      Close : ts' -> pure (fst inner, ts')
      rest -> Left $ "expected ‘)’, found " <> show rest
  rest -> Left $ "expected an expression, found " <> show rest

alt :: [Token] -> Either String (Syntax, [Token])
alt = \case
  Hole : Arrow : ts -> expr ts
  rest -> Left $ "expected ‘_ ->’ starting an alternative, found " <> show rest

bind :: [Token] -> Either String ((String, Syntax), [Token])
bind = \case
  Ident v : Equals : ts | v `notElem` keywords -> first (v,) <$> expr ts
  rest -> Left $ "expected ‘‹name› =’ starting a binding, found " <> show rest

-- | One or more @p@, separated by @;@.
sepBy :: ([Token] -> Either String (a, [Token])) -> [Token] -> Either String ([a], [Token])
sepBy p ts = do
  item <- p ts
  case snd item of
    Semi : ts' -> first (fst item :) <$> sepBy p ts'
    rest -> pure ([fst item], rest)

toExp :: Syntax -> TH.Exp
toExp = \case
  Leaf ->
    con 'Plugins.Lit [TH.VarE 'Plugins.mkLitInt64 `TH.AppE` TH.LitE (TH.IntegerL 0)]
  Lam v body -> con 'Plugins.Lam [str v, toExp body]
  App f a -> con 'Plugins.App [toExp f, toExp a]
  Let v rhs body -> con 'Plugins.Let [con 'Plugins.NonRec [str v, toExp rhs], toExp body]
  Letrec bnds body ->
    con
      'Plugins.Let
      [ con
          'Plugins.Rec
          [TH.ListE $ (\(v, rhs) -> TH.TupE [Just (str v), Just (toExp rhs)]) <$> bnds],
        toExp body
      ]
  Case scrut alts ->
    con
      'Plugins.Case
      [ toExp scrut,
        -- NOTE: The case binder is never observed — `Recursion.inExpr`
        --       matches `Plugins.Case scrut _ _ alts` — so the grammar has no
        --       way to name it and it is always this.
        str "wild",
        TH.VarE 'Plugins.unitTy,
        TH.ListE $
          (\a -> con 'Plugins.Alt [TH.ConE 'Plugins.DEFAULT, TH.ListE [], toExp a])
            <$> alts
      ]
  Cast e ->
    con
      'Plugins.Cast
      [toExp e, TH.VarE 'Plugins.mkRepReflCo `TH.AppE` TH.VarE 'Plugins.unitTy]

con :: TH.Name -> [TH.Exp] -> TH.Exp
con name = foldl' TH.AppE (TH.ConE name)

str :: String -> TH.Exp
str = TH.LitE . TH.StringL
