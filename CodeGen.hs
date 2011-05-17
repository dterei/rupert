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

module CodeGen (gen) where

import Control.Monad.Trans
import qualified Control.Monad.State as State
import Data.Int
import Data.Map((!))
import qualified Data.Map as Map
import LLVM.Core
import LLVM.ExecutionEngine

import Parser

type Scope = Map.Map String (Value Int32)
type MyState r a = State.StateT Scope (CodeGenFunction r) a

expr2fun :: Expr -> MyState r (Value Int32)
expr2fun (Var name) = do state <- State.get
                         return $ state ! name
expr2fun (Val v) = return (valueOf ((fromInteger v) :: Int32))
expr2fun (Add e1 e2) = do a <- expr2fun e1
                          b <- expr2fun e2
                          lift $ add a b
expr2fun (Mult e1 e2) = do a <- expr2fun e1
                           b <- expr2fun e2
                           lift $ mul a b

ast2fun :: Stmt -> MyState (Int32) ()
ast2fun (Seq stmts) = sequence_ $ map ast2fun stmts
ast2fun (Return e) = do fun <- expr2fun e
                        lift $ ret fun
ast2fun (Assign name e) = do state <- State.get
                             fun <- expr2fun e
                             let state' = Map.insert name fun state
                             State.put state'
                             return ()

outerAst2fun :: Stmt -> CodeGenModule (Function (Int32 -> Int32 -> IO Int32))
outerAst2fun ast = createFunction ExternalLinkage $ \i1 i2 ->
                       let initState = Map.fromList [("i1", i1), ("i2", i2)]
                       in State.evalStateT (ast2fun ast) initState

gen :: Stmt -> String -> IO ()
gen ast outputPath = do
  mod <- newModule
  defineModule mod (outerAst2fun ast)
  writeBitcodeToFile outputPath mod
