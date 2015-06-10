module Storytime.Parsing where

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
readIdent = T.pack <$> liftM2 (:) letter (many $ alphaNum <|> oneOf "-/")

readIdent' :: Parser T.Text
readIdent' = ws *> readIdent

tok :: String -> Parser T.Text
tok t = T.pack <$> (ws *> string t)

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
readHeader = char '*' *> readIdent' <* eolOrEof

readShebang :: Parser ()
readShebang = string "#!" *> tillEndOfLine *> eolOrEof

readInt :: Parser Int
readInt = do
  ws
  sign <- option id (char '-' *> pure negate)
  num <- many1 digit
  return . sign $ read num

readValue :: Parser Value
readValue = try number <|> var
  where
    number = EInt <$> readInt
    var = EVar <$> readIdent'

readEqual :: Parser BExpr
readEqual = Equal <$> readValue <* tok "=" <*> readValue

readLessThan :: Parser BExpr
readLessThan = LessThan <$> readValue <* tok "<" <*> readValue

readGreaterThan :: Parser BExpr
readGreaterThan = GreaterThan <$> readValue <* tok ">" <*> readValue

readAction :: Parser Act
readAction = (try increment) <|> (try decrement) <|> assignment
  where
    increment = pure Inc <* tok "+" <*> readIdent
    decrement = pure Dec <* tok "-" <*> readIdent
    assignment = Assign <$> readIdent' <* tok "=" <*> readIExpr

readLink :: Parser Link
readLink = do
  char '['
  target <- readIdent'
  cond <- option Nothing (Just <$> readCondClause)
  act <- option [] readActionClause
  tok "]:"
  ws
  title <- tillEndOfLine
  return $ Link target title cond act
  where
    readCondClause = tok "|" *> ws *> (try readBExpr) <* ws
    readActionClause = tok "," *> ws *> sepBy1 readAction (tok ",")

parens = between (tok "(") (tok ")")

readBExpr :: Parser BExpr
readBExpr = buildExpressionParser table readTerm
  where
    readTerm = try readEqual <|>
               try readLessThan <|>
               try readGreaterThan <|>
               parens readBExpr
    table = [ [ Prefix (try (tok "~" *> pure Not)) ]
            , [ Infix (try (tok "&&" *> pure And)) AssocLeft
              , Infix (try (tok "||" *> pure Or)) AssocLeft ] ]

readVal :: Parser IExpr
readVal = Val <$> readValue

readIExpr :: Parser IExpr
readIExpr = buildExpressionParser table readTerm
  where
    readTerm = try readVal <|> parens readIExpr
    table = [ [ Infix (try (tok "*" *> pure Mult)) AssocLeft ]
            , [ Infix (try (tok "+" *> pure Plus)) AssocLeft
              , Infix (try (tok "-" *> pure Minus)) AssocLeft ] ]

peek p = void (lookAhead $ try p)

readDynText :: Parser DynText
readDynText = manyTill (try readCond <|> try readVar <|> readLit) terminator
  where
    terminator = peek (endOfLine *> readLink) <|>
                 peek (endOfLine *> readHeader) <|>
                 peek eof

readText :: Parser T.Text
readText = T.pack <$> manyTill anyChar (lookAhead $ try terminator)
  where
    terminator = peek (endOfLine *> readLink) <|>
                 peek (endOfLine *> readHeader) <|>
                 peek (oneOf "{}") <|>
                 peek readVar <|>
                 peek eof

readLit :: Parser Span
readLit = Lit <$> readText

readCond :: Parser Span
readCond = pure Cond
           <* char '{'
           <*> readBExpr
           <* char ':'
           <*> readText
           <* char '}'

readVar :: Parser Span
readVar = pure Var <* char '$' <*> readIdent

readSection :: Parser Section
readSection = Sect
              <$> readHeader
              <*> readDynText
              <*> option [] (endOfLine *> many (readLink <* spaces))

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

