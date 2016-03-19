{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser where

import           Control.Lens
import           Data.List             (genericReplicate)
import           Data.Scientific
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Megaparsec       as M
import qualified Text.Megaparsec.Lexer as L
import Data.Function

data Lift
    = Lift
    { _name    :: Text
    , _setList :: [Set]
    } deriving (Eq, Show)

data Set
    = Set
    { _weight :: Scientific
    , _reps   :: Integer
    } deriving (Eq, Show)

makeLenses ''Lift
makeLenses ''Set

data Session = Session [Lift] deriving (Eq, Show)

parse :: Parsec Text a -> String -> Text -> Either ParseError a
parse = M.parse

session :: Parsec Text Session
session = Session <$> liftParser `sepBy` eol

liftParser :: Parsec Text Lift
liftParser = Lift <$> liftName <*> liftSets

liftName :: Parsec Text Text
liftName = T.pack <$> (space' *> someTill anyChar (char ':') <* space)

liftSets :: Parsec Text [Set]
liftSets = concat <$> setLine `sepBy` eol

setLine :: Parsec Text [Set]
setLine = do
    weight' <- decimal <?> "Attempting to get weight"
    firstOff <- repsxsets
    reps'repeats <- fix $ \go -> repsxsets >>= \case
                                               (1,1) -> return []
                                               this  -> (this :) <$> go
    let sets' = firstOff : reps'repeats >>= uncurry (flip genericReplicate)
    return . map (Set weight') $ sets'

repsxsets :: Parsec Text (Integer, Integer)
repsxsets = do
    reps_ <- xThenInt <?> "Trying to get the reps..."
    repeats <- xThenInt <* skipComma <?> "Trying to get the repeats."
    return (reps_, repeats)

xThenInt :: Parsec Text Integer
xThenInt = option 1 $ try (space' >> skipChar 'x' >> integer)

lexeme :: Parsec Text a -> Parsec Text a
lexeme = L.lexeme space'

comma :: Parsec Text Char
comma = lexeme (char ',')

decimal :: Parsec Text Scientific
decimal = either (`scientific` 0) fromFloatDigits <$> number

float :: Parsec Text Double
float = lexeme L.float

number :: Parsec Text (Either Integer Double)
number = lexeme L.number

integer :: Parsec Text Integer
integer = lexeme L.integer

skipChar :: Char -> Parsec Text ()
skipChar = lexeme . skipMany . char'

skipComma :: Parsec Text ()
skipComma = skipChar ','

space' :: Parsec Text ()
space' = skipMany (oneOf " \t")
