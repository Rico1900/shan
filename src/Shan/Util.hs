module Shan.Util (
  LiteratureCase(..),
  Parser,
  symbolS,
  symbolW
) where
  
import Shan.Ast.Diagram (Bound)
import Text.Megaparsec qualified as Mega
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec.Char.Lexer (symbol)
import Text.Megaparsec.Char (space)
import Text.Megaparsec (single)
import Control.Applicative.Combinators (many)
import Control.Monad (void)

data LiteratureCase = LiteratureCase
  { name :: String,
    path :: FilePath,
    bound :: Bound
  }

type Parser a = Mega.Parsec Void Text a

-- match all tailing blank symbols
symbolS :: Text -> Parser Text
symbolS = symbol space

-- match tailing spaces
symbolW :: Text -> Parser Text
symbolW = symbol (void . many $ single ' ')