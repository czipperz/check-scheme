module Parse (parseProgram) where

import Grammar
import Pos
import Text.Parsec

parseProgram :: String -> String -> Either String Program
parseProgram sourceName = either (Left . show) return . parse program sourceName

program = do
  spaces
  exprs <- expressions
  eof
  return exprs

expressions = noExpressions <|> headExpression
  where noExpressions = eof >> return []
        headExpression = do
          expr <- expression
          spaces
          fmap (expr:) expressions

expression = do
  spaces
  pos <- getPosition
  let pos' = Pos (sourceName pos) (sourceLine pos) (sourceColumn pos)
  fmap (Tag pos') (try constant <|> try variable <|> list <?> "expression")

constant = fmap Constant (constantValue <?> "constant")
  where constantValue = try boolean <|> try number <|> try character <|> stringConstant

boolean = try t <|> f <?> "boolean"
  where t = string "#t" >> return (Boolean True)
        f = string "#f" >> return (Boolean False)

number = integer <?> "number"
integer = fmap Integer value <?> "integer"
  where value = digits <|> do
          minus <- char '-'
          fmap (\x -> -x) digits
        digits = read <$> many1 digit

character = do
  string "#\\" <?> "character"
  fmap Character $ try space <|> try newline <|> anyChar
    where space = string "space" >> return ' '
          newline = string "newline" >> return '\n'

stringConstant = do
  char '"' <?> "string"
  contents <- many stringCharacter
  char '"'
  return . String . concat $ contents
    where stringCharacter = try escapedQuote <|> try escapedBackslash <|> normalStringCharacter
          escapedQuote = string "\\\"" >> return "\""
          escapedBackslash = string "\\\\" >> return "\\"
          normalStringCharacter = fmap return $ noneOf ['\\', '"']

variable = fmap Variable identifier
  where identifier = basicIdentifier <|> string "+" <|> string "-" <|> string "..." <?> "identifier"
        basicIdentifier = do
          initial <- initial
          subsequents <- many subsequent
          return (initial:subsequents)
        initial = letter <|> oneOf "!$%&*/:<=>?~_^"
        subsequent = initial <|> digit <|> oneOf ".+-"

list = do
  char '(' <?> "list"
  fmap List $ emptyList <|> listBody
    where emptyList = do
            char ')' <?> "end of list"
            return []
          listBody = do
            expr <- expression
            spaces
            let endOfList = do
                  char ')' <?> "end of list"
                  return [expr]
            endOfList <|> (fmap (expr:) listBody)
