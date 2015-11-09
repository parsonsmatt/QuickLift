{-# LANGUAGE TemplateHaskell #-}

module Parser where

import           Control.Lens
import           Data.List             (genericReplicate)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

data Lift
  = Lift
  { _name    :: Text
  , _setList :: [Set]
  } deriving (Eq, Show)

data Set
  = Set
  { _weight :: Integer
  , _reps   :: Integer
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
  s <- Set <$> integer <*> xThenInt
  repeats <- xThenInt <* skipComma
  return $ genericReplicate repeats s

xThenInt :: Parsec Text Integer
xThenInt = option 1 $ try (skipChar 'x' >> integer)

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
