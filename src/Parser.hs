{-# LANGUAGE TemplateHaskell #-}

module Parser where

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

data Lift
  = Lift
  { _name :: Text
  , _setList :: [Set]
  } deriving (Eq, Show)

data Set
  = Set
  { _weight :: Integer
  , _reps :: Integer
  } deriving (Eq, Show)

makeLenses ''Lift
makeLenses ''Set

data Session = Session [Lift] deriving Show

session :: Parsec Text Session
session = Session <$> many liftParser

liftParser :: Parsec Text Lift
liftParser = Lift <$> liftName <*> liftSets

liftName :: Parsec Text Text
liftName = T.pack <$> someTill anyChar (char ':') <* space

liftSets :: Parsec Text [Set]
liftSets = concat <$> many setLine

setLine :: Parsec Text [Set]
setLine = do
  weight <- integer
  reps <- option 1 $ try $ do
    skipChar 'x'
    integer
  sets <- option 1 $ try $ do
    skipChar 'x'
    integer
  skipComma
  return $ replicate (fromInteger sets) $ Set weight reps


lexeme :: Parsec Text a -> Parsec Text a
lexeme = L.lexeme space

comma :: Parsec Text Char
comma = lexeme (char ',')

integer :: Parsec Text Integer
integer = lexeme L.integer

skipChar :: Char -> Parsec Text ()
skipChar = lexeme . skipMany . char'

skipComma :: Parsec Text ()
skipComma = skipChar ','
