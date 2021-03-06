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
tillEndOfLine = T.pack <$> manyTill anyChar (lookAhead $ try eolOrEof)

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
readMeta = M.fromList <$> many (readLine <* eolOrEof) <?> "metadata"
  where
    readLine = pure (,)
               <* char '%'
               <*> readIdent'
               <* char ':'
               <* ws
               <*> tillEndOfLine

readActionClause :: Parser [Act]
readActionClause = tok "," *> ws *> sepBy1 readAction (tok ",") <?> "action definition"

readCondClause :: Parser BExpr
readCondClause = tok "|" *> ws *> try readBExpr <* ws <?> "condition definition"

readHeader :: Parser (Tag, [Act])
readHeader = do
  char '*'
  tag <- readIdent'
  acts <- option [] readActionClause
  eolOrEof
  return (tag, acts)
  <?> "header"

readShebang :: Parser ()
readShebang = string "#!" *> tillEndOfLine *> eolOrEof <?> "shebang"

readInt :: Parser Integer
readInt = do
  ws
  sign <- option id (char '-' *> pure negate)
  num <- many1 digit
  return . sign $ read num

readValue :: Parser Value
readValue = try number <|> var
  where
    number = EInt <$> readInt <?> "integer"
    var = EVar <$> readIdent' <?> "variable"

readEqual :: Parser BExpr
readEqual = Equal <$> readValue <* tok "=" <*> readValue

readLessThan :: Parser BExpr
readLessThan = LessThan <$> readValue <* tok "<" <*> readValue

readGreaterThan :: Parser BExpr
readGreaterThan = GreaterThan <$> readValue <* tok ">" <*> readValue

readAction :: Parser Act
readAction = try increment <|> try decrement <|> assignment <?> "action"
  where
    increment = pure Inc <* tok "+" <*> readIdent
    decrement = pure Dec <* tok "-" <*> readIdent
    assignment = Assign <$> readIdent' <* tok "=" <*> readIExpr

readLink :: Parser Link
readLink = do
  char '['
  target' <- readIdent'
  cond' <- option Nothing (Just <$> readCondClause)
  act <- option [] readActionClause
  tok "]:"
  ws
  title' <- tillEndOfLine
  return $ Link target' title' cond' act
  <?> "link"

parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

readBExpr :: Parser BExpr
readBExpr = buildExpressionParser table readTerm <?> "boolean expression"
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
readIExpr = buildExpressionParser table readTerm <?> "integer expression"
  where
    readTerm = try readVal <|>
               parens readIExpr
    table = [ [ Infix (try (tok "*" *> pure Mult)) AssocLeft ]
            , [ Infix (try (tok "+" *> pure Plus)) AssocLeft
              , Infix (try (tok "-" *> pure Minus)) AssocLeft ] ]

peek :: Parser a -> Parser ()
peek p = void (lookAhead $ try p)

embed :: Parser a -> Parser a
embed = between (string "${") (char '}')

readLit :: Parser Span
readLit = do
  notFollowedBy header'
  notFollowedBy link'
  notFollowedBy $ char '$'
  x <- anyChar
  xs <- manyTill anyChar terminator
  return . Lit . T.pack $ x:xs
  where
    header' = endOfLine *> readHeader
    link' = endOfLine *> readLink
    terminator = peek header' <|> peek link' <|> peek (char '$') <|> peek eof

readCond :: Parser Span
readCond = embed (Cond <$> readBExpr <* tok ":" <*> readText)

readVar :: Parser Span
readVar = embed (Var <$> readIdent)

readText :: Parser T.Text
readText = T.pack <$> many1 (noneOf "}")

readDynText :: Parser DynText
readDynText = many (try readCond <|> try readVar <|> readLit)

readSection :: Parser Section
readSection = do
  (tag, acts) <- readHeader
  text <- readDynText
  links <- option [] (endOfLine *> many (readLink <* spaces))
  return $ Sect tag acts text links

readStory :: Parser Story
readStory = do
  optional readShebang
  meta' <- readMeta
  spaces
  sects' <- many1 readSection
  let sectMap = M.fromList [(tag s, s) | s <- sects']
  eof
  return $ Story meta' (head sects') sectMap

loadStory :: FilePath -> IO (Either ParseError Story)
loadStory p = withFile p ReadMode $ \h -> do
  source <- IO.hGetContents h
  return $ parse readStory p source

