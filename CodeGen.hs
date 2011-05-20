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

data RpValue = RpInt32 (Value Int32)
             | RpFun0 (Function (IO Int32))
             | RpFun1 (Function (Int32 -> IO Int32))
             | RpFun2 (Function (Int32 -> Int32 -> IO Int32))

-- name resolution works on a stack of scopes with globals at the bottom
type Scopes = [Scope]

-- a scope maps from names to values
type Scope = Map.Map String RpValue

-- lookup a name in a scope stack
scopeLookup :: String -> Scopes -> Maybe RpValue
scopeLookup name (scope:rest) =
    case Map.lookup name scope of
        Just v -> Just v
        Nothing -> scopeLookup name rest
scopeLookup name [] = Nothing

-- lookup a name in a scope stack, but throw an error if it's not found
scopeGet :: String -> Scopes -> RpValue
scopeGet name scopes =
    case scopeLookup name scopes of
        Just v -> v
        Nothing -> error $ "variable '" ++ name ++ "' not in scope"

-- set a name in a scope stack
-- If it exists, replace the old value. Otherwise, create a new one at the innermost scope.
scopeSet :: String -> RpValue -> Scopes -> Scopes
scopeSet name value scopes@(scope:rest) =
    case scopeLookup name scopes of
        Just _ -> scopeSet' name value scopes
        Nothing -> (Map.insert name value scope):rest
    where scopeSet' name value scopes@(scope:rest) =
              case Map.lookup name scope of
                Just _ -> (Map.insert name value scope):rest
                Nothing -> scope:(scopeSet' name value rest)

-- Push a new scope onto the stack
scopePush :: Scopes -> Scopes
scopePush scopes = Map.empty : scopes

-- Pop a scope from the stack
scopePop :: Scopes -> Scopes
scopePop (scope:rest) = rest

type MyState r a = State.StateT Scopes (CodeGenFunction r) a

-- generate the LLVM code to evaluate an expression
genExpr :: Expr -> MyState r (Value Int32)
genExpr (Var name) = do
    scopes <- State.get
    case scopeGet name scopes of
      RpInt32 v -> return v
      _ -> error "Not the expected type"
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
genExpr (Call name args) = do
    scopes <- State.get
    case scopeGet name scopes of
      RpFun0 fun -> lift $ call fun
      RpFun1 fun -> case args of
        [expr1] -> do arg1 <- genExpr expr1
                      lift $ call fun arg1
      RpFun2 fun -> case args of
        [expr1, expr2] -> do arg1 <- genExpr expr1
                             arg2 <- genExpr expr2
                             lift $ call fun arg1 arg2
      _ -> error "Not the expected type"


-- generate the LLVM code for a function body
genFunBody :: Stmt -> MyState (Int32) ()
genFunBody (Seq stmts) =
    sequence_ $ map genFunBody stmts
genFunBody (Return e) = do
    fun <- genExpr e
    lift $ ret fun
genFunBody (Assign name e) = do
    fun <- genExpr e
    State.modify $ scopeSet name (RpInt32 fun)
    return ()
genFunBody (JustExpr e@(Call _ _)) = do
    fun <- genExpr e
    return ()


-- generate a function that takes no arguments
genFun0 :: String -> [String] -> Stmt ->
           CodeGenModule (Function (IO Int32))
genFun0 name ([]) ast = do
    putchar <- newNamedFunction ExternalLinkage "putchar" :: TFunction (Int32 -> IO Int32)
    createNamedFunction ExternalLinkage name $
        let initState = [Map.fromList [("putchar", RpFun1 putchar)]]
        in State.evalStateT (genFunBody ast) initState

-- generate a function that takes one argument
genFun1 :: String -> [String] -> Stmt ->
           CodeGenModule (Function (Int32 -> IO Int32))
genFun1 name ([arg1]) ast =
    createNamedFunction ExternalLinkage name $ \i1 ->
       let initState = [Map.fromList [(arg1, RpInt32 i1)]]
       in State.evalStateT (genFunBody ast) initState

-- generate a function that takes two arguments
genFun2 :: String -> [String] -> Stmt ->
           CodeGenModule (Function (Int32 -> Int32 -> IO Int32))
genFun2 name ([arg1, arg2]) ast =
    createNamedFunction ExternalLinkage name $ \i1 i2 ->
       let initState = [Map.fromList [(arg1, RpInt32 i1), (arg2, RpInt32 i2)]]
       in State.evalStateT (genFunBody ast) initState

-- generate the functions found at the top level of the file
genModule :: Stmt -> CodeGenModule ()
genModule (Seq stmts) =
    sequence_ $ map genModule stmts
genModule (Assign name (Fun args@[] stmt)) =
    genFun0 name args stmt >> return ()
genModule (Assign name (Fun args@[a1] stmt)) =
    genFun1 name args stmt >> return ()
genModule (Assign name (Fun args@[a1,a2] stmt)) =
    genFun2 name args stmt >> return ()

-- write the bytecode for the given ast to a file
gen :: Stmt -> String -> IO ()
gen ast outputPath = do
  mod <- newModule
  defineModule mod (genModule ast)
  writeBitcodeToFile outputPath mod
