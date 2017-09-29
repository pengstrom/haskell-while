module While.Parse where

import While.Types

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef = emptyDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedOpNames =
    [ "+", "-", "*", ":=", "<=", "and", "not" ]
  , Token.reservedNames =
    [ "if"
    , "while"
    , "do"
    , "else"
    , "skip"
    , "true"
    , "false"
    , "not"
    , "and"
    ]
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
integer    = Token.integer lexer
semi       = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
  <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
  list@(x:xs) <- sepBy1 statement' semi
  return $ if length list == 1 then x else Comp list

statement' :: Parser Stmt
statement' = assignStmt
  <|> ifStmt
  <|> whileStmt
  <|> skipStmt

ifStmt :: Parser Stmt
ifStmt = If <$> (reserved "if" *> bexpr) <*> (reserved "then" *> statement) <*> (reserved "else" *> statement)

assignStmt :: Parser Stmt
assignStmt = Ass <$> identifier <*> (reservedOp ":=" *> aexpr)

whileStmt :: Parser Stmt
whileStmt = While <$> (reserved "while" *> bexpr) <*> (reserved "do" *> statement)

skipStmt :: Parser Stmt
skipStmt = reserved "skip" *> pure Skip

aOperators =
  [ 
    [ Infix (reservedOp "*" *> pure Mult) AssocLeft
    ]
  , [ Infix (reservedOp "+" *> pure Add) AssocLeft
    , Infix (reservedOp "-" *> pure Sub) AssocLeft
    ]
  ]

bOperators =
  [ 
    [ Prefix (reservedOp "not" *> pure Neg)
    ]
  , [ Infix (reservedOp "and" *> pure Con) AssocLeft
    ]
  ]

aexpr :: Parser AExpr
aexpr = buildExpressionParser aOperators aTerm

bexpr :: Parser BExpr
bexpr = buildExpressionParser bOperators bTerm

aTerm = parens aexpr
  <|> fmap Var identifier
  <|> fmap Lit integer

bTerm = parens bexpr
  <|> reserved "true"  *> pure TT
  <|> reserved "false" *> pure FF
  <|> rexpr

rexpr :: Parser BExpr
rexpr = Eq <$> aexpr <*> (reservedOp "==" *> aexpr)
  <|> Lte <$> aexpr <*> (reservedOp "<=" *> aexpr)

parseWhile :: String -> Stmt
parseWhile src =
  case parse whileParser "" src of
    Left e -> error $ show e
    Right r -> r
