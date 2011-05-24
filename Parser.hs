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

module Parser (
      Expr(..)
    , Stmt(..)
    , stmtToStr
    , exprToStr
    , parseRupert
    ) where

import Control.Applicative((<*))
import Data.List
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
          | StringVal String
          | Add Expr Expr
          | Mult Expr Expr
          | Fun [String] Stmt
          | Call String [Expr]
    deriving (Show)

data Stmt = Assign String Expr
          | Return Expr
          | Seq [Stmt]
          | JustExpr Expr -- for stand-alone function calls
    deriving (Show)

exprToStr i (Var name) = name
exprToStr i (Val v) = show v
exprToStr i (StringVal v) = show v
exprToStr i (Add e1 e2) =
    "(" ++ (exprToStr i e1) ++ " + " ++ (exprToStr i e2) ++ ")"
exprToStr i (Mult e1 e2) =
    "(" ++ (exprToStr i e1) ++ " * " ++ (exprToStr i e2) ++ ")"
exprToStr i (Fun args s) =
    let spaceBeforeArgs = map (\a -> ' ':a) args
        commaSepArgs = concat $ intersperse "," spaceBeforeArgs
    in "fun" ++ commaSepArgs ++ ":\n" ++
       (stmtToStr' (i + 1) s)
exprToStr i (Call name args) =
    let stringArgs = map (\a -> exprToStr i a) args
        commaSepArgs = concat $ intersperse ", " stringArgs
    in name ++ "(" ++ commaSepArgs ++ ")"

stmtToStr s = stmtToStr' 0 s
stmtToStr' i (Assign name e@(Fun _ _)) =
    indent i ++ name ++ " = " ++ (exprToStr i e)
stmtToStr' i (Assign name e) =
    indent i ++ name ++ " = " ++ (exprToStr i e) ++ "\n"
stmtToStr' i (Return e) =
    indent i ++ "return " ++ (exprToStr i e) ++ "\n"
stmtToStr' i (JustExpr e) =
    indent i ++ (exprToStr i e) ++ "\n"
stmtToStr' i (Seq stmts) =
    concat $ map (stmtToStr' i) stmts
indent i = replicate (i * 4) ' '


def = emptyDef{ commentLine = "#"
              , opStart = oneOf "+*"
              , opLetter = oneOf "+*"
              , reservedNames = ["fun", "return"]
              , reservedOpNames = ["+", "*"]
              }
tokenParser = makeTokenParser def
m_commaSep = IT.commaSep tokenParser
-- decimal doesn't eat whitespace, but natural does?
m_decimal = m_lexeme (IT.decimal tokenParser)
m_identifier = IT.identifier tokenParser
m_lexeme = IT.lexeme tokenParser
m_parens = IT.parens tokenParser
m_reserved = IT.reserved tokenParser
m_reservedOp = IT.reservedOp tokenParser
m_semiSep = IT.semiSep tokenParser
m_semiOrNewLineSep = IT.semiOrNewLineSep tokenParser
m_stringLiteral = IT.stringLiteral tokenParser
m_whiteSpace = IT.whiteSpace tokenParser

exprparser =     do name <- m_identifier
                    args <- m_parens $ m_commaSep exprparser
                    return (Call name args)
             <|> buildExpressionParser table term
             <|> do m_reserved "fun"
                    args <- m_commaSep m_identifier
                    m_reservedOp ":"
                    s <- P.block stmtparser
                    return (Fun args s)
table = [ [Infix (m_reservedOp "*" >> return Mult) AssocLeft]
        , [Infix (m_reservedOp "+" >> return Add) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Val m_decimal
       <|> fmap StringVal m_stringLiteral
       <|> fmap Var m_identifier

mainparser = m_whiteSpace >> stmtparser <* eof
stmtparser = fmap Seq (m_semiOrNewLineSep stmt1)
    where stmt1 =    -- look-ahead in case this is a function call
                     do v <- try ( do { v <- m_identifier;
                                        m_reservedOp "=";
                                        return v } )
                        e <- exprparser
                        return (Assign v e)
                 <|> do m_reserved "return"
                        p <- exprparser
                        return (Return p)
                 <|> do p <- exprparser
                        return (JustExpr p)

p input = case P.parse mainparser "" input of
           Left error -> print error
           Right ans -> print ans

q inputPath = do input <- readFile inputPath
                 case P.parse mainparser inputPath input of
                   Left error -> print error
                   Right ans -> print ans

parseRupert :: String -> IO (Either ParseError Stmt)
parseRupert inputPath = do input <- readFile inputPath
                           return (P.parse mainparser inputPath input)

main = do argv <- System.Environment.getArgs
          q (head argv)
