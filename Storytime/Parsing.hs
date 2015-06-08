module Storytime.Parsing (readStory, loadStory) where

import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad
import System.IO
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Text
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import Storytime.Types

tillEndOfLine :: Parser T.Text
tillEndOfLine = T.pack <$> manyTill anyChar (lookAhead $ try endOfLine)

eolOrEof :: Parser ()
eolOrEof = void endOfLine <|> eof

ws :: Parser ()
ws = optional ws1

ws1 :: Parser ()
ws1 = void $ many1 (oneOf " \t")

readIdent :: Parser T.Text
readIdent = T.pack <$> liftM2 (:) letter (many $ alphaNum <|> oneOf "-")

readIdent' :: Parser T.Text
readIdent' = ws *> readIdent

readMeta :: Parser Meta
readMeta = M.fromList <$> many (readLine <* eolOrEof)
  where
    readLine = pure (,)
               <* char '%'
               <*> readIdent'
               <* char ':'
               <* ws
               <*> tillEndOfLine

readHeader :: Parser Tag
readHeader = char '#' *> ws *> readIdent <* eolOrEof

readShebang :: Parser ()
readShebang = string "#!" *> tillEndOfLine *> eolOrEof

readAction :: Parser Act
readAction = do
  mod <- oneOf "+-~"
  var <- readIdent
  return $ case mod of
    '+' -> Set var
    '-' -> Unset var
    '~' -> Flip var

readLink :: Parser Link
readLink = do
  char '['
  target <- readIdent'
  cond <- option Nothing (Just <$> readCondClause)
  spaces
  act <- option [] readActionClause
  string "]:"
  ws
  title <- tillEndOfLine
  return $ Link target title cond act
  where
    readCondClause = char '|' *> ws *> readExpr
    readActionClause = char ',' *> ws *> sepBy1 readAction ws1

readExpr :: Parser Expr
readExpr = buildExpressionParser table readTerm
  where
    readTerm = (Var <$> (ws *> readIdent <* ws)) <|> parens readExpr
    table = [[Prefix (char '~' *> pure Not)],
             [Infix (ws *> string "&&" *> ws *> pure And) AssocLeft,
              Infix (ws *> string "||" *> ws *> pure Or) AssocLeft]]
    parens = between (char '(') (char ')')

readDynText :: Parser DynText
readDynText = manyTill (readCond <|> readLit) (lookAhead $ try terminator)
  where
    terminator = void (endOfLine *> readLink) <|>
                 void (endOfLine *> readHeader) <|>
                 void eof

readText :: Parser T.Text
readText = T.pack <$> manyTill anyChar (lookAhead $ try terminator)
  where
    terminator = void (endOfLine *> readLink) <|>
                 void (endOfLine *> readHeader) <|>
                 void (oneOf "{}") <|>
                 void eof

readLit :: Parser Span
readLit = Lit <$> readText

readCond :: Parser Span
readCond = pure Cond
           <* char '{'
           <*> readExpr
           <* char ':'
           <*> readText
           <* char '}'

readSection :: Parser Section
readSection = Sect
              <$> readHeader
              <*> readDynText
              <* endOfLine
              <*> many (readLink <* spaces)

readStory :: Parser Story
readStory = do
  optional readShebang
  meta <- readMeta
  spaces
  sects <- many1 readSection
  let sects' = M.fromList [(tag s, s) | s <- sects]
  eof
  return $ Story meta (head sects) sects'

loadStory :: FilePath -> IO (Either ParseError Story)
loadStory p = withFile p ReadMode $ \h -> do
  source <- IO.hGetContents h
  return $ parse readStory p source

