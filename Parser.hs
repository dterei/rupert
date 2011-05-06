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
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

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
TokenParser{ identifier = m_identifier
           , reserved = m_reserved
           , operator = m_operator
           , reservedOp = m_reservedOp
           , charLiteral = m_charLiteral
           , stringLiteral = m_stringLiteral
           , natural = m_natural
           , integer = m_integer
           , float = m_float
           , naturalOrFloat = m_naturalOrFloat
           , decimal = m_decimalNoWs
           , hexadecimal = m_hexadecimal
           , octal = m_octal
           , symbol = m_symbol
           , lexeme = m_lexeme
           , whiteSpace = m_whiteSpace
           , parens = m_parens
           , braces = m_braces
           , angles = m_angles
           , brackets = m_brackets
           , semi = m_semi
           , comma = m_comma
           , colon = m_colon
           , dot = m_dot
           , semiSep = m_semiSep
           , semiSep1 = m_semiSep1
           , commaSep = m_commaSep
           , commaSep1 = m_commaSep1
           } = tokenParser
-- decimal doesn't eat whitespace, but natural does?
m_decimal = m_decimalNoWs <* m_whiteSpace

exprparser :: Parser Expr
exprparser = buildExpressionParser table term
table = [ [Infix (m_reservedOp "*" >> return Mult) AssocLeft]
        , [Infix (m_reservedOp "+" >> return Add) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Val m_decimal
       <|> fmap Var m_identifier

mainparser = m_whiteSpace >> stmtparser <* eof
  where stmtparser = fmap Seq (m_semiSep stmt1)
        stmt1 = do v <- m_identifier
                   m_reservedOp "="
                   e <- exprparser
                   return (Assign v e)

p input = case parse mainparser "" input of
           Left error -> print error
           Right ans -> print ans
