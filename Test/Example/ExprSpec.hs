{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances, LambdaCase #-}
module Example.ExprSpec where

import Prelude hiding (takeWhile, product)

import Test

import PLParser hiding (pending)
import PLParser.Expected

import PLPrinter hiding (between)

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.List (intercalate, intersperse)
import GHC.Generics (Generic)
import Data.Functor

newtype Name = Name Text
  deriving Eq

newtype Type = Type Text
  deriving Eq

{-
 - This example should be a more complex Lambda-Calculus like expression that
 - covers more fiddly functionality like:
 - - Nested & optional enclosing tokens (like parenthesis)
 - - Multiple repeated sub-parsers
 - - Etc
 -}

data Abstraction = Abstraction Name Type SubExpr
  deriving Eq

data Application = Application SubExpr SubExpr
  deriving Eq

data Binding = Binding Name
  deriving Eq

data Literal = Literal Int
  deriving Eq

data Product = Product [SubExpr]
  deriving Eq

data SubExpr = SubExpr Expr
  deriving Eq

-- Expr is a lambda calculus with integer literals and products that abstracts
-- and binds variables by name.
data Expr
  = ExprAbstraction Abstraction
  | ExprApplication Application
  | ExprBinding Binding
  | ExprLiteral Literal
  | ExprProduct Product
  deriving Eq

-- TODO: Could probably 'deriving via these'?
instance Show Expr where show = Text.unpack . write
instance Show Abstraction where show = Text.unpack . write
instance Show Application where show = Text.unpack . write
instance Show Binding where show = Text.unpack . write
instance Show Literal where show = Text.unpack . write
instance Show Product where show = Text.unpack . write
instance Show SubExpr where show = Text.unpack . write

instance Document Expr where document = text . write
instance Document Abstraction where document = text . write
instance Document Application where document = text . write
instance Document Binding where document = text . write
instance Document Literal where document = text . write
instance Document Product where document = text . write
instance Document SubExpr where document = text . write

propExprParses :: forall a. (Parse a, Arbitrary a, Show a, Document a, Eq a) => String -> (a -> Expr) -> Spec
propExprParses test f = prop test $ \x -> (runParser parse . write . f $ x) `passes` x

spec :: Spec
spec = describe "Expr parsers" $ do
  propExprParses "literals" ExprLiteral
  propExprParses "bindings" ExprBinding
  propExprParses "abstraction" ExprAbstraction
  propExprParses "application" ExprApplication
  propExprParses "product" ExprProduct

{- Misc -}

space :: Parser ()
space = charIs ' '

alpha :: [Char]
alpha = ['a'..'z'] <> ['A'..'Z']

isAlpha :: Predicate Char
isAlpha = Predicate (\c -> elem c alpha) (ExpectPredicate (descriptiveLabel "ALPHA") Nothing)

spaceSeparated :: Parser a -> Parser [a]
spaceSeparated p = (:) <$> p <*> many (textIs " " *> p)

sepBy1 :: Parser () -> Parser a -> Parser [a]
sepBy1 sep p = (:) <$> p
                   <*> alternatives [ try $ sep *> sepBy1 sep p
                                    , pure []
                                    ]

{- Parsers -}

class Parse a where
  parse :: Parser a

instance Parse Name where
  --parse = Name <$> takeWhile1 isAlpha
  parse = Name <$> alternatives
    [ try $ textIs "Foo" $> "Foo"
    , try $ textIs "Bar" $> "Bar"
    , textIs "Baz" $> "Baz"
    ]

instance Parse Type where
  --parse = Type <$> takeWhile1 isAlpha
  parse = Type <$> alternatives
    [ try $ textIs "Int" $> "Int"
    , try $ textIs "String" $> "String"
    , textIs "Bool" $> "Bool"
    ]

instance Parse Literal where
  parse = Literal <$> do
    digits <- reverse <$> some singleDigit
    pure . foldl (\total (digit,base) -> total + digit * base) 0
         . zip digits
         . iterate (* 10)
         $ 1

instance Parse Binding where
  parse = Binding <$> parse

instance Parse Product where
  parse = Product <$> sepBy1 (charIs ',') parse

instance Parse Abstraction where
  parse = Abstraction <$> (parse <* space) <*> (parse <* space) <*> parse

instance Parse Application where
  parse = Application <$> (parse <* space) <*> parse

instance Parse SubExpr where
  parse = SubExpr <$> alternatives
    [ try (between (charIs '(') complexExpr (charIs ')'))
    , simpleExpr
    ]

instance Parse Expr where
  parse = alternatives
    [ try complexExpr
    , simpleExpr
    ]

-- A single digit. Better implemented as a predicate and map.
singleDigit :: Parser Int
singleDigit = label (descriptiveLabel "digit") $ try $ alternatives
  [ try $ charIs '0' $> 0
  , try $ charIs '1' $> 1
  , try $ charIs '2' $> 2
  , try $ charIs '3' $> 3
  , try $ charIs '4' $> 4
  , try $ charIs '5' $> 5
  , try $ charIs '6' $> 6
  , try $ charIs '7' $> 7
  , try $ charIs '8' $> 8
  , charIs '9' $> 9
  ]

simpleExpr :: Parser Expr
simpleExpr = alternatives
  [ try (ExprLiteral <$> parse)
  , ExprBinding <$> parse
  ]

complexExpr :: Parser Expr
complexExpr = alternatives
  [ try $ ExprAbstraction <$> (charIs '\\' *> parse)
  , try $ ExprApplication <$> (charIs '@'  *> parse)
  , try $ ExprProduct           <$> (charIs '*'  *> parse)
  ]

{- Generators -}

scaleHalf :: Gen a -> Gen a
scaleHalf = scale (`div` 2)

instance Arbitrary Name where
  arbitrary = Name <$> elements ["Foo", "Bar", "Baz"]

instance Arbitrary Abstraction where
  arbitrary = scaleHalf
    $ Abstraction
   <$> arbitrary
   <*> arbitrary
   <*> scaleHalf arbitrary

instance Arbitrary Application where
  arbitrary = scaleHalf
    $ Application
   <$> scaleHalf arbitrary
   <*> scaleHalf arbitrary

instance Arbitrary Binding where
  arbitrary = Binding <$> arbitrary

instance Arbitrary Literal where
  arbitrary = Literal . getPositive <$> arbitrary

instance Arbitrary Product where
  arbitrary = scaleHalf $ fmap Product . scaleHalf . listOf1 $ arbitrary

instance Arbitrary Type where
  arbitrary = Type . Text.pack <$> elements ["Int", "String", "Bool"]

instance Arbitrary SubExpr where
  arbitrary = SubExpr <$> arbitrary

instance Arbitrary Expr where
  arbitrary = arbitraryExpr

-- Simple expressions which don't nest
arbitrarySimple :: Gen Expr
arbitrarySimple = oneof
  [ ExprLiteral <$> arbitrary
  , ExprBinding <$> arbitrary
  ]

-- Expressions which may nest other expressions
arbitraryComplex :: Gen Expr
arbitraryComplex = oneof
  [ scaleHalf $ ExprAbstraction <$> arbitrary
  , scaleHalf $ ExprApplication <$> arbitrary
  , scaleHalf $ ExprProduct     <$> arbitrary
  ]

arbitraryExpr :: Gen Expr
arbitraryExpr = do
  size <- getSize
  if size < 5
    then frequency [ (4, arbitrarySimple)
                   , (1, arbitraryComplex)
                   ]
    else frequency [ (1, arbitrarySimple)
                   , (4, arbitraryComplex)
                   ]

{- Printers -}

class Write a where
  write :: a -> Text

instance Write Literal where
  write (Literal i) = Text.pack . show $ i

instance Write Binding where
  write (Binding (Name n)) = n

instance Write Product where
  write (Product es) = "*" <> (mconcat . intersperse ", " . fmap write $ es)

instance Write Application where
  write (Application f x) = "@" <> write f <> " " <> write x

instance Write Abstraction where
  write (Abstraction (Name n) (Type t) e) = "\\" <> n <> " " <> t <> " " <> write e

instance Write SubExpr where
  write (SubExpr e) = case e of
    ExprAbstraction a
      -> "(" <> write a <> ")"

    ExprApplication a
      -> "(" <> write a <> ")"

    ExprProduct p
      -> "(" <> write p <> ")"

    ExprBinding b
      -> write b

    ExprLiteral l
      -> write l

instance Write Expr where
  write = \case
    ExprAbstraction a
      -> write a

    ExprApplication a
      -> write a

    ExprBinding b
      -> write b

    ExprLiteral l
      -> write l

    ExprProduct p
      -> write p

