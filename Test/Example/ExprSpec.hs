{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances, LambdaCase #-}
module Example.ExprSpec where

import Prelude hiding (takeWhile, product)

import Test

import PLParser
import PLParser.Expected

import PLPrinter hiding (between)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (intersperse)
import Data.Functor

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
    [ "Foo"
    , "Bar"
    , "Baz"
    ]

instance Write Name where
  write (Name n) = n

instance Parse Name where
  parse = Name <$> alternatives
    [ textIs "Foo" $> "Foo"
    , textIs "Bar" $> "Bar"
    , textIs "Baz" $> "Baz"
    ]


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
  parse = Type <$> alternatives
    [ textIs "Int"    $> "Int"
    , textIs "String" $> "String"
    , textIs "Bool"   $> "Bool"
    ]


{- Abstraction: \Foo String (* 1 2) -}

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
    , write name, " "
    , write typ , " "
    , write bodyExpr
    ]

instance Parse Abstraction where
  parse = charIs '\\'
        *> (Abstraction
       <$> token parse
       <*> token parse
       <*> token parse
           )


{- Application -}

data Application = Application Expr Expr
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
    [ "@"
    , write f, " "
    , write x
    ]

instance Parse Application where
  parse = textIs "@"
        *> (Application
       <$> token parse
       <*> token parse
           )

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
  parse = Binding <$> token parse

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
  parse = Literal <$> token natural

{- Product -}
data Product = Product [Expr]
  deriving Eq

instance Show Product where
  show = Text.unpack . write

instance Document Product where
  document = text . write

instance Arbitrary Product where
  arbitrary = scaleHalf $ fmap Product . scaleHalf . listOf1 $ arbitrary

instance Write Product where
  write (Product es) = mconcat
    [ "*"
    , mconcat . intersperse " " . fmap write $ es
    ]

instance Parse Product where
  parse = textIs "*"
        *> (Product <$> some (token parse))

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
  parse = alternatives
    [ try (ExprLiteral <$> parse)
    , ExprBinding <$> parse
    ]


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
  write e = mconcat
    ["("
    , case e of
        ExprAbstraction a
          -> write a

        ExprApplication a
          -> write a

        ExprProduct p
          -> write p
    , ")"
    ]

instance Parse ComplexExpr where
  parse = textIs "("
        *> (alternatives
             [ try (ExprAbstraction <$> parse)
             , try (ExprApplication <$> parse)
             , ExprProduct <$> parse
             ]
           )
        <* textIs ")"


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
  parse = alternatives
    [ try (ExprComplex <$> parse)
    , ExprSimple <$> parse
    ]


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
propExprParses test f = prop test $ \x -> (runParser parse . write . f $ x) `passes` x

spec :: Spec
spec = describe "Expr parsers" $ do
  propExprParses "literals"    (ExprSimple  . ExprLiteral)
  propExprParses "bindings"    (ExprSimple  . ExprBinding)
  propExprParses "abstraction" (ExprComplex . ExprAbstraction)
  propExprParses "application" (ExprComplex . ExprApplication)
  propExprParses "product"     (ExprComplex . ExprProduct)

{- Misc -}

alpha :: [Char]
alpha = ['a'..'z'] <> ['A'..'Z']

isAlpha :: Predicate Char
isAlpha = Predicate (\c -> elem c alpha) (ExpectPredicate (descriptiveLabel "ALPHA") Nothing)

-- TODO: Hmm
sepBy1 :: Parser () -> Parser a -> Parser [a]
sepBy1 sep p = (:) <$> p
                   <*> many (sep *> p)

scaleHalf :: Gen a -> Gen a
scaleHalf = scale (`div` 2)

