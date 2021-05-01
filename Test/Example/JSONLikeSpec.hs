{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveGeneric, FlexibleInstances #-}
module Example.JSONLikeSpec
  ( spec
  )
  where

import Prelude hiding (takeWhile)

import Test

import PLParser
import PLParser.Expected

import PLPrinter

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Text as Text
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Functor

-- | Similar to JSON in structure
data JSONLike
  = NaturalValue Int
  | StringValue Text
  | BoolValue Bool
  | ArrayValue [JSONLike]
  | ObjectValue (Map Text JSONLike)
  deriving (Eq, Generic)

instance Show JSONLike where
  show = Text.unpack . writeJSONLike

instance Document JSONLike where
  document = text . writeJSONLike

spec :: Spec
spec = describe "JSON(like) parsers" $ do
  -- 0,1,2,3,4,5,6,7,8,9
  prop "single digits" $ do
    d <- generate $ elements [0 .. 9]
    (runParser singleDigit . Text.pack . show $ d)
      `passes` d

  -- 0,1,..., 10,11,...
  prop "naturals" $ \positive ->
    (runParser naturalValue . writeJSONLike . NaturalValue . getPositive $ positive)
      `passes` (getPositive positive)

  -- "foo", "bar"
  prop "strings" $ do
    txt <- generate arbitraryStringValue
    (runParser stringValue . writeJSONLike . StringValue $ txt)
      `passes` txt

  -- True, False
  prop "booleans" $ do
    b <- generate arbitraryBoolValue
    (runParser boolValue . writeJSONLike . BoolValue $ b)
      `passes` b

  -- 0, 10, "foo", True
  prop "scalars" $ do
    scalar <- generate arbitraryScalarValue
    (runParser scalarValue . writeJSONLike $ scalar)
      `passes` scalar

  -- [], [0,10,"foo",True,[],{}]
  prop "arrays" $ do
    array <- generate $ ArrayValue <$> sized arbitraryArrayValue
    (runParser (ArrayValue <$> arrayValue) (writeJSONLike array))
      `passes` array

  -- {}, {key0:0, key1:10, key2:"foo", key3:True, key4:[], key5:{}}
  prop "objects" $ do
    object <- generate $ ObjectValue <$> sized arbitraryObjectValue
    (runParser (ObjectValue <$> objectValue) (writeJSONLike object))
      `passes` object

  -- [], {}
  prop "composites" $ do
    composite <- generate . sized $ arbitraryCompositeValue
    (runParser compositeValue (writeJSONLike composite))
      `passes` composite

  prop "json" $ do
    json <- generate . sized $ arbitraryJSONLike
    (runParser jsonLike (writeJSONLike json))
      `passes` json

{- Parsers -}

-- A single digit. Better implemented as a predicate and map.
singleDigit :: Parser Int
singleDigit = alternatives
  [ textIs "0" $> 0
  , textIs "1" $> 1
  , textIs "2" $> 2
  , textIs "3" $> 3
  , textIs "4" $> 4
  , textIs "5" $> 5
  , textIs "6" $> 6
  , textIs "7" $> 7
  , textIs "8" $> 8
  , textIs "9" $> 9
  ]

-- | A natural number is a positive integer made up of 0 or many digits.
naturalValue :: Parser Int
naturalValue = do
  digits <- reverse <$> some singleDigit
  pure . foldl (\total (d,base) -> total + d * base) 0
       . zip digits
       . iterate (* 10)
       $ 1

alphaNumeric :: [Char]
alphaNumeric = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

isAlphaNumeric :: Predicate Char
isAlphaNumeric = Predicate (\c -> elem c alphaNumeric) (ExpectPredicate (descriptiveLabel "ALPHANUMERIC") Nothing)

-- | Text enclosed in double quotation marks.
stringValue :: Parser Text
stringValue = (charIs '"' *> takeWhile isAlphaNumeric <* charIs '"')

-- | True or False.
boolValue :: Parser Bool
boolValue = alternatives
  [ textIs "True"  $> True
  , textIs "False" $> False
  ]

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = (:) <$> p <*> many (textIs "," *> p)

-- | Many JSONLike elements, enclosed in square brackets.
arrayValue :: Parser [JSONLike]
arrayValue = alternatives
  [ try $ textIs "[]" $> []
  , charIs '[' *> commaSeparated jsonLike <* charIs ']'
  ]

-- | Many key-values, enclosed in curly braces.
objectValue :: Parser (Map Text JSONLike)
objectValue = Map.fromList <$> alternatives
  [ textIs "{}" $> []
  , charIs '{' *> commaSeparated keyValue <* charIs '}'
  ]

-- | A key : value
keyValue :: Parser (Text, JSONLike)
keyValue = (,) <$> (key <* charIs ':') <*> jsonLike

-- | A key (not containing special characters)
key :: Parser Text
key = takeWhile1 isAlphaNumeric

-- | Values which don't contain other values.
scalarValue :: Parser JSONLike
scalarValue = alternatives $
  [ NaturalValue <$> naturalValue
  , BoolValue    <$> boolValue
  , StringValue  <$> stringValue
  ]

-- | Values which may contain other values.
compositeValue :: Parser JSONLike
compositeValue = alternatives $
  [ try $ ObjectValue <$> objectValue
  , ArrayValue  <$> arrayValue
  ]

-- | A JSONLike thing is one of several alternatives.
jsonLike :: Parser JSONLike
jsonLike = alternatives $
  [ try scalarValue
  , compositeValue
  ]

{- Generators -}

arbitraryNaturalValue :: Gen Int
arbitraryNaturalValue = getPositive <$> arbitrary

alphaNum :: Gen Char
alphaNum = elements alphaNumeric

arbitraryStringValue :: Gen Text
arbitraryStringValue = Text.pack <$> listOf alphaNum

arbitraryBoolValue :: Gen Bool
arbitraryBoolValue = oneof [pure False, pure True]

arbitraryArrayValue :: Int -> Gen [JSONLike]
arbitraryArrayValue = scale (`div` 2) . listOf . arbitraryJSONLike . (`div` 2)

arbitraryObjectValue :: Int -> Gen (Map Text JSONLike)
arbitraryObjectValue = fmap Map.fromList . scale (`div` 2) . listOf . arbitraryKeyValue . (`div` 2)

arbitraryKeyValue :: Int -> Gen (Text, JSONLike)
arbitraryKeyValue size = (,) <$> arbitraryKey <*> arbitraryJSONLike size

arbitraryKey :: Gen Text
arbitraryKey = Text.pack <$> listOf1 alphaNum

arbitraryScalarValue :: Gen JSONLike
arbitraryScalarValue = oneof
  [ BoolValue    <$> arbitraryBoolValue
  , NaturalValue <$> arbitraryNaturalValue
  , StringValue  <$> arbitraryStringValue
  ]

arbitraryCompositeValue :: Int -> Gen JSONLike
arbitraryCompositeValue size = oneof
  [ ArrayValue  <$> arbitraryArrayValue size
  , ObjectValue <$> arbitraryObjectValue size
  ]

-- Small size => more simple values
-- Large size => more composite values
arbitraryJSONLike :: Int -> Gen JSONLike
arbitraryJSONLike size
  | size < 5  = frequency [ (4, arbitraryScalarValue)
                          , (1, arbitraryCompositeValue size)
                          ]
  | otherwise = frequency [ (1, arbitraryScalarValue)
                          , (4, arbitraryCompositeValue size)
                          ]

instance Arbitrary JSONLike where
  arbitrary = sized arbitraryJSONLike
  shrink = genericShrink


{- Printers -}

writeJSONNatural :: Int -> Text
writeJSONNatural = Text.pack . show

writeJSONString :: Text -> Text
writeJSONString s = mconcat
  [ "\""
  , mconcat . fmap writeJSONCharacter . Text.unpack $ s
  , "\""
  ]

writeJSONCharacter :: Char -> Text
writeJSONCharacter c
  | (_predicate isAlphaNumeric) c
  = Text.singleton c

  | otherwise
  = error "Writing non-alpha-numeric characters not implemented" -- Because we'd have to mess around with an escaping strategy.

writeJSONBool :: Bool -> Text
writeJSONBool b
  | b = "True"
  | otherwise = "False"

writeJSONArray :: [JSONLike] -> Text
writeJSONArray as = mconcat
  ["["
  , Text.intercalate "," . fmap writeJSONLike $ as
  ,"]"
  ]

writeJSONObject :: Map Text JSONLike -> Text
writeJSONObject os = mconcat
  ["{"
  , Text.intercalate "," . fmap (\(k,v) -> k <> ":" <> writeJSONLike v) . Map.toList $ os
  , "}"
  ]

-- | Render a JSONLike to a textual representation that _should_ be able to be
-- parsed back by the jsonLike Parser.
writeJSONLike :: JSONLike -> Text
writeJSONLike j = case j of
  NaturalValue n
    -> writeJSONNatural n

  StringValue s
    -> writeJSONString s

  BoolValue b
    -> writeJSONBool b

  ArrayValue as
    -> writeJSONArray as

  ObjectValue os
    -> writeJSONObject os

