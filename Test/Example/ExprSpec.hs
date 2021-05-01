{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances, LambdaCase, MultiWayIf #-}
{-|
Module      : Example.ExprSpec
Copyright   : (c) Samuel A. Yallop, 2021
Maintainer  : syallop@gmail.com
Stability   : experimental

An incomplete - in terms of not actually being useful - expression language, with the aim of exercising:
- The Monad interface (rather than Applicative, even when Monad isn't technically needed).
- Character parsers (which don't backtrack, rather than text parsers which do)
- Lookahead (with textual errors, rather than backtracking with runs on hope)
-}
module Example.ExprSpec
  ( spec
  )
  where

import Prelude hiding (takeWhile, product)

import Test

import PLParser
import PLParser.Char

import PLPrinter hiding (between)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Applicative hiding (some, many)

spec :: Spec
spec = describe "Expr parsers" $ do
  propExprParses "literals"    (ExprSimple  . ExprLiteral)
  propExprParses "bindings"    (ExprSimple  . ExprBinding)
  propExprParses "abstraction" (ExprComplex . ExprAbstraction)
  propExprParses "application" (ExprComplex . ExprApplication)
  propExprParses "product"     (ExprComplex . ExprProduct)

  propExprParses "expressions" id

{- Classes -}

-- | Things which can be written as parsable text
class Write a where
  write :: a -> Text

-- | Things which can be parsed from text
class Parse a where
  parse :: Parser a

{- Names -}

newtype Name = Name Text
  deriving Eq

instance Arbitrary Name where
  arbitrary = Name <$> elements
    [ "foo"
    , "bar"
    , "baz"
    ]

instance Write Name where
  write (Name n) = n

instance Parse Name where
  parse = peekChar >>= \c -> if
    | c == 'f' -> (textIs "foo" $> Name "foo") <|> fail "The name 'foo' after seeing an 'f'."
    | c == 'b' -> (textIs "bar" $> Name "bar") <|> (textIs "baz" $> Name "baz") <|> fail "Saw name beginning with b, expected bar or baz"
    | otherwise -> fail "Expected a name: foo, bar or baz"

{- Types -}

newtype Type = Type Text
  deriving Eq

instance Arbitrary Type where
  arbitrary = Type . Text.pack <$> elements
    [ "Int"
    , "String"
    , "Bool"
    ]

instance Write Type where
  write (Type t) = t

instance Parse Type where
  parse = peekChar >>= \c -> if
    | c == 'I' -> (textIs "Int"    $> Type "Int")    <|> fail "The type 'Int' after seeing an 'I'."
    | c == 'S' -> (textIs "String" $> Type "String") <|> fail "The type 'String' after seeing an 'S'."
    | c == 'B' -> (textIs "Bool"   $> Type "Bool")   <|> fail "The type 'Bool' after seeing a 'B'."
    | otherwise -> fail "Expected a type: Int, String, Bool"

{- Abstraction: \foo : String. * 1 2 -}

data Abstraction = Abstraction Name Type Expr
  deriving Eq

instance Show Abstraction where
  show = Text.unpack . write

instance Document Abstraction where
  document = text . write

instance Arbitrary Abstraction where
  arbitrary = scaleHalf
            $ Abstraction
           <$> arbitrary
           <*> arbitrary
           <*> scaleHalf arbitrary

instance Write Abstraction where
  write (Abstraction name typ bodyExpr) = mconcat
    [ "\\"
    , write name
    , " : "

    , write typ
    , ". "

    , write bodyExpr
    ]

instance Parse Abstraction where
  parse = peekChar >>= \case
    '\\' -> do charIs '\\'
               f  <- parse
               charIs ' '
               charIs ':'
               charIs ' '

               ty <- parse
               charIs '.'
               charIs ' '

               x  <- parse
               pure $ Abstraction f ty x

    _ -> fail "An Abstraction beginning with \\"


{- Application -}

-- @ (foo) (1)
data Application = Application Binding Expr
  deriving Eq

instance Show Application where
  show = Text.unpack . write

instance Document Application where
  document = text . write

instance Arbitrary Application where
  arbitrary = scaleHalf
    $ Application
   <$> scaleHalf arbitrary
   <*> scaleHalf arbitrary

instance Write Application where
  write (Application f x) = mconcat
    [ "@ ("
    , write f, ") "

    , "("
    , write x
    , ")"
    ]

instance Parse Application where
  parse = peekChar >>= \case
    '@'
      -> do charIs '@'
            charIs ' '
            charIs '('
            f <- parse
            charIs ')'
            charIs ' '
            charIs '('
            x <- parse
            charIs ')'
            pure $ Application f x

    _ -> fail "An Application beginning with @"

{- Binding -}
data Binding = Binding Name
  deriving Eq

instance Show Binding where
  show = Text.unpack . write

instance Document Binding where
  document = text . write

instance Arbitrary Binding where
  arbitrary = Binding <$> arbitrary

instance Write Binding where
  write (Binding n) = write n

instance Parse Binding where
  parse = Binding <$> parse

{- Literal -}
data Literal = Literal Int
  deriving Eq

instance Show Literal where
  show = Text.unpack . write

instance Document Literal where
  document = text . write

instance Arbitrary Literal where
  arbitrary = Literal . getPositive <$> arbitrary

instance Write Literal where
  write (Literal i) = Text.pack . show $ i

instance Parse Literal where
  parse = Literal <$> natural

{- Product -}

-- *(foo)(1)
data Product = Product (NonEmpty Expr)
  deriving Eq

instance Show Product where
  show = Text.unpack . write

instance Document Product where
  document = text . write

instance Arbitrary Product where
  arbitrary = scaleHalf $ fmap (Product . NE.fromList) . scaleHalf . listOf1 $ arbitrary

instance Write Product where
  write (Product es) = mconcat
    [ "*("
    , mconcat . NE.toList . fmap (\e -> write e <> ",") $ es
    , ")"
    ]

instance Parse Product where
  parse = peekChar >>= \case
    '*'
      -> do charIs '*'
            charIs '('
            arguments <- NE.fromList <$> some (parse <* charIs ',')
            charIs ')'
            pure $ Product arguments

    _ -> fail "A Product beginning with *"

{- SimpleExpr -}
data SimpleExpr
  = ExprLiteral Literal
  | ExprBinding Binding
  deriving Eq

instance Show SimpleExpr where
  show = Text.unpack . write

instance Document SimpleExpr where
  document = text . write

instance Arbitrary SimpleExpr where
  arbitrary = oneof
    [ ExprLiteral <$> arbitrary
    , ExprBinding <$> arbitrary
    ]

instance Write SimpleExpr where
  write = \case
    ExprLiteral l
      -> write l

    ExprBinding b
      -> write b

instance Parse SimpleExpr where
  parse = peekChar >>= \c -> if
    | c `elem` ['0'..'9'] -> ExprLiteral <$> parse
    | c `elem` ['f','b']  -> ExprBinding <$> parse
    | otherwise -> fail "Expected a literal number or a binding (foo, bar, baz)"

{- ComplexExpr -}
data ComplexExpr
  = ExprAbstraction Abstraction
  | ExprApplication Application
  | ExprProduct Product
  deriving Eq

instance Show ComplexExpr where
  show = Text.unpack . write

instance Document ComplexExpr where
  document = text . write

instance Arbitrary ComplexExpr where
  arbitrary = oneof
    [ ExprAbstraction <$> scaleHalf arbitrary
    , ExprApplication <$> scaleHalf arbitrary
    , ExprProduct     <$> scaleHalf arbitrary
    ]

instance Write ComplexExpr where
  write = \case
    ExprAbstraction a
      -> write a

    ExprApplication a
      -> write a

    ExprProduct p
      -> write p

instance Parse ComplexExpr where
  parse = peekChar >>= \case
    '\\'
      -> ExprAbstraction <$> parse

    '*'
      -> ExprProduct <$> parse


    '@'
      -> ExprApplication <$> parse

    _
      -> fail "Expected product, abstraction or application."

{- Expr -}
-- Expr is a lambda calculus with integer literals and products that abstracts
-- and binds variables by name.
data Expr
  = ExprSimple  SimpleExpr
  | ExprComplex ComplexExpr
  deriving Eq

instance Show Expr where
  show = Text.unpack . write

instance Document Expr where
  document = text . write

instance Arbitrary Expr where
  arbitrary = do
    size <- getSize
    frequency [ (if size < 5 then 4 else 1, ExprSimple  <$> arbitrary)
              , (if size < 5 then 1 else 4, ExprComplex <$> arbitrary)
              ]

instance Write Expr where
  write = \case
    ExprSimple e
      -> write e

    ExprComplex e
      -> write e

instance Parse Expr where
  parse = peekChar >>= \c -> if
    | c `elem` ['\\', '@', '*']          -> ExprComplex <$> parse
    | c `elem` (['0'..'9'] <> ['f','b']) -> ExprSimple  <$> parse
    | otherwise -> fail "Expected either a simple or complex expression"

propExprParses
  :: forall a
   . ( Parse a
     , Arbitrary a
     , Show a
     , Document a
     , Eq a
     )
  => String
  -> (a -> Expr)
  -> Spec
propExprParses test f = prop test $ \x ->
  let expr   = f x
      parser = parse <* end
      input  = write expr
   in runParser parser input `passes` expr

{- Misc -}

scaleHalf :: Gen a -> Gen a
scaleHalf = scale (`div` 2)

