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

-- generate the LLVM code to evaluate an expression
genExpr :: Expr -> MyState r (Value Int32)
genExpr (Var name) = do
    state <- State.get
    return $ state ! name
genExpr (Val v) =
    return $ valueOf ((fromInteger v) :: Int32)
genExpr (Add e1 e2) = do
    a <- genExpr e1
    b <- genExpr e2
    lift $ add a b
genExpr (Mult e1 e2) = do
    a <- genExpr e1
    b <- genExpr e2
    lift $ mul a b

-- generate the LLVM code for a function body
genFunBody :: Stmt -> MyState (Int32) ()
genFunBody (Seq stmts) =
    sequence_ $ map genFunBody stmts
genFunBody (Return e) = do
    fun <- genExpr e
    lift $ ret fun
genFunBody (Assign name e) = do
    fun <- genExpr e
    State.modify $ Map.insert name fun
    return ()

-- generate a function that takes no arguments
genFun0 :: [String] -> Stmt -> CodeGenModule (Function (IO Int32))
genFun0 ([]) ast =
    createFunction ExternalLinkage $
        let initState = Map.fromList []
        in State.evalStateT (genFunBody ast) initState

-- generate a function that takes one argument
genFun1 :: [String] -> Stmt -> CodeGenModule (Function (Int32 -> IO Int32))
genFun1 ([arg1]) ast =
    createFunction ExternalLinkage $ \i1 ->
       let initState = Map.fromList [(arg1, i1)]
       in State.evalStateT (genFunBody ast) initState

-- generate a function that takes two arguments
genFun2 :: [String] -> Stmt -> CodeGenModule (Function (Int32 -> Int32 -> IO Int32))
genFun2 ([arg1, arg2]) ast =
    createFunction ExternalLinkage $ \i1 i2 ->
       let initState = Map.fromList [(arg1, i1), (arg2, i2)]
       in State.evalStateT (genFunBody ast) initState

-- generate the functions found at the top level of the file
genModule :: Stmt -> CodeGenModule ()
genModule (Seq stmts) =
    sequence_ $ map genModule stmts
genModule (Fun args@[] stmt) =
    genFun0 args stmt >> return ()
genModule (Fun args@[a1] stmt) =
    genFun1 args stmt >> return ()
genModule (Fun args@[a1,a2] stmt) =
    genFun2 args stmt >> return ()

-- write the bytecode for the given ast to a file
gen :: Stmt -> String -> IO ()
gen ast outputPath = do
  mod <- newModule
  defineModule mod (genModule ast)
  writeBitcodeToFile outputPath mod
