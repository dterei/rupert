{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Word
import Data.Map((!))
import qualified Data.Map as Map
import LLVM.Core
import LLVM.ExecutionEngine

import Parser

data RpValue = RpInt32 (Value Int32)
             | forall n. (Nat n) => RpString (Global (Array n Word8))
             | RpFun0 (Function (IO Int32))
             | RpFun1 (Function (Int32 -> IO Int32))
             | RpFun2 (Function (Int32 -> Int32 -> IO Int32))
             | RpStrFun (Function (Ptr Word8 -> IO Int32))

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

asInt32 :: RpValue -> Value Int32
asInt32 (RpInt32 v) = v

getStringPtr :: RpValue -> CodeGenFunction r (Value (Ptr Word8))
getStringPtr (RpString v) = getElementPtr v (0::Word32, (0::Word32, ()))

-- generate the LLVM code to evaluate an expression
genExpr :: Expr -> MyState r RpValue
genExpr (Var name) = do
    scopes <- State.get
    return $ scopeGet name scopes
genExpr (Val v) =
    return $ RpInt32 $ valueOf ((fromInteger v) :: Int32)
genExpr (StringVal v) = do
    scopes <- State.get
    return $ scopeGet v scopes
genExpr (Add e1 e2) = do
    a <- genExpr e1
    b <- genExpr e2
    res <- lift $ add (asInt32 a) (asInt32 b)
    return $ RpInt32 res
genExpr (Mult e1 e2) = do
    a <- genExpr e1
    b <- genExpr e2
    res <- lift $ mul (asInt32 a) (asInt32 b)
    return $ RpInt32 res
genExpr (Call name args) = do
    scopes <- State.get
    case scopeGet name scopes of
      RpFun0 fun -> do
        res <- lift $ call fun
        return $ RpInt32 res
      RpFun1 fun -> case args of
        [expr1] -> do arg1 <- genExpr expr1
                      res <- lift $ call fun (asInt32 arg1)
                      return $ RpInt32 res
      RpFun2 fun -> case args of
        [expr1, expr2] -> do arg1 <- genExpr expr1
                             arg2 <- genExpr expr2
                             res <- lift $ call fun (asInt32 arg1) (asInt32 arg2)
                             return $ RpInt32 res
      RpStrFun fun -> case args of
        [expr1] -> do arg1 <- genExpr expr1
                      stringPtr <- lift $ getStringPtr arg1
                      res <- lift $ call fun stringPtr
                      return $ RpInt32 res
      _ -> error "Not the expected type"


-- generate the LLVM code for a function body
genFunBody :: Stmt -> MyState (Int32) ()
genFunBody (Seq stmts) =
    sequence_ $ map genFunBody stmts
genFunBody (Return e) = do
    fun <- genExpr e
    lift $ ret (asInt32 fun)
genFunBody (Assign name e) = do
    fun <- genExpr e
    State.modify $ scopeSet name fun
    return ()
genFunBody (JustExpr e@(Call _ _)) = do
    fun <- genExpr e
    return ()


-- generate a function that takes no arguments
genFun0 :: String -> [String] -> Stmt -> Scopes ->
           CodeGenModule (Function (IO Int32))
genFun0 name ([]) ast scopes = do
    createNamedFunction ExternalLinkage name $
        State.evalStateT (genFunBody ast) scopes

-- generate a function that takes one argument
genFun1 :: String -> [String] -> Stmt -> Scopes ->
           CodeGenModule (Function (Int32 -> IO Int32))
genFun1 name ([arg1]) ast scopes =
    createNamedFunction ExternalLinkage name $ \i1 ->
       let scopes'  = scopeSet arg1 (RpInt32 i1) scopes
       in State.evalStateT (genFunBody ast) scopes'

-- generate a function that takes two arguments
genFun2 :: String -> [String] -> Stmt -> Scopes ->
           CodeGenModule (Function (Int32 -> Int32 -> IO Int32))
genFun2 name ([arg1, arg2]) ast scopes =
    createNamedFunction ExternalLinkage name $ \i1 i2 ->
       let scopes'  = scopeSet arg1 (RpInt32 i1) scopes
           scopes'' = scopeSet arg2 (RpInt32 i2) scopes'
       in State.evalStateT (genFunBody ast) scopes''

-- Create an LLVM global from a string literal
intern :: String -> CodeGenModule RpValue
intern string = withStringNul string $ \s -> return (RpString s)

-- generate the functions found at the top level of the file
genModule :: Stmt -> CodeGenModule ()
genModule ast = do
  putchar <- newNamedFunction ExternalLinkage "putchar" :: TFunction (Int32 -> IO Int32)
  puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Int32)
  let strings = extractStrings ast
  cstrings <- sequence $ map intern strings
  -- TODO: it's a botch to mix string literals in with variable names
  let initScope = [Map.empty,
                   Map.fromList [("putchar", RpFun1 putchar),
                                 ("puts", RpStrFun puts)],
                   Map.fromList $ zip strings cstrings]
  let genModule' ast =
        case ast of
          Seq stmts ->
              sequence_ $ map genModule' stmts
          Assign name (Fun args@[] stmt) ->
              genFun0 name args stmt initScope >> return ()
          Assign name (Fun args@[a1] stmt) ->
              genFun1 name args stmt initScope >> return ()
          Assign name (Fun args@[a1,a2] stmt) ->
              genFun2 name args stmt initScope >> return ()
  genModule' ast

-- Extract the string literals from an AST
extractStrings :: Stmt -> [String]
extractStrings stmt =
    case stmt of
        Assign _ e -> eS e
        Return e   -> eS e
        Seq stmts  -> concat $ map extractStrings stmts
        JustExpr e -> eS e
    where eS expr = case expr of
              StringVal string -> [string]
              Fun _ stmt       -> extractStrings stmt
              Call _ exprs     -> concat $ map eS exprs
              _                -> []

-- write the bytecode for the given ast to a file
gen :: Stmt -> String -> IO ()
gen ast outputPath = do
  mod <- newModule
  defineModule mod (genModule ast)
  writeBitcodeToFile outputPath mod
