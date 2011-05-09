{-
Copyright (c) 2011 Stanford University

Permission to use, copy, modify, and distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL AUTHORS BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT,
OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
-}


module Parser where

import Control.Applicative((<*))
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.IndentParser as P
import qualified Text.ParserCombinators.Parsec.IndentParser.Token as IT

data Expr = Var String
          | Val Integer
          | Add Expr Expr
          | Mult Expr Expr
    deriving (Show)

data Stmt = Assign String Expr
          | Seq [Stmt]
    deriving (Show)

def = emptyDef{ opStart = oneOf "+*"
              , opLetter = oneOf "+*"
              , reservedOpNames = ["+", "*"]
              }
tokenParser = makeTokenParser def
-- decimal doesn't eat whitespace, but natural does?
m_decimal = m_lexeme (IT.decimal tokenParser)
m_identifier = IT.identifier tokenParser
m_lexeme = IT.lexeme tokenParser
m_parens = IT.parens tokenParser
m_reservedOp = IT.reservedOp tokenParser
m_semiSep = IT.semiSep tokenParser
m_semiOrNewLineSep = IT.semiOrNewLineSep tokenParser
m_whiteSpace = IT.whiteSpace tokenParser

exprparser = buildExpressionParser table term
table = [ [Infix (m_reservedOp "*" >> return Mult) AssocLeft]
        , [Infix (m_reservedOp "+" >> return Add) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Val m_decimal
       <|> fmap Var m_identifier

mainparser = m_whiteSpace >> stmtparser <* eof
  where stmtparser = fmap Seq (m_semiOrNewLineSep stmt1)
        stmt1 = do v <- m_identifier
                   m_reservedOp "="
                   e <- exprparser
                   return (Assign v e)

p input = case P.parse mainparser "" input of
           Left error -> print error
           Right ans -> print ans

q inputPath = do input <- readFile inputPath
                 case P.parse mainparser inputPath input of
                   Left error -> print error
                   Right ans -> print ans

main = do argv <- System.Environment.getArgs
          q (head argv)
