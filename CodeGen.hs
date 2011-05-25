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
import Environment

type MyState r a = State.StateT Environment (CodeGenFunction r) a

asInt32 :: RpValue -> Value Int32
asInt32 (RpInt32 v) = v

getStringPtr :: RpValue -> CodeGenFunction r (Value (Ptr Word8))
getStringPtr (RpString v) = getElementPtr v (0::Word32, (0::Word32, ()))

-- generate the LLVM code to evaluate an expression
genExpr :: Expr -> MyState r RpValue
genExpr (Var name) = do
    env <- State.get
    return $ envGetVar name env
genExpr (Val v) =
    return $ RpInt32 $ valueOf ((fromInteger v) :: Int32)
genExpr (StringVal v) = do
    env <- State.get
    return $ envGetString v env
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
    env <- State.get
    case envGetVar name env of
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
    State.modify $ envSetVar name fun
    return ()
genFunBody (JustExpr e@(Call _ _)) = do
    fun <- genExpr e
    return ()

-- generate code for a function:
-- bind arguments for function parameters and defer to genFunBody
genFun :: String -> [String] -> Stmt -> Environment -> CodeGenModule ()
genFun name args ast env =
  case envGetVar name env of
    RpFun0 fun -> defineFunction fun $
       State.evalStateT (genFunBody ast) env
    RpFun1 fun -> defineFunction fun $ \i1 ->
       let [arg1] = args
           env' = env -$
                  envSetVar arg1 (RpInt32 i1)
       in State.evalStateT (genFunBody ast) env'
    RpFun2 fun -> defineFunction fun $ \i1 i2 ->
       let [arg1, arg2] = args
           env' = env -$
                  envSetVar arg1 (RpInt32 i1) -$
                  envSetVar arg2 (RpInt32 i2)
       in State.evalStateT (genFunBody ast) env'

-- Create an LLVM global from a string literal
intern :: String -> CodeGenModule RpValue
intern string = withStringNul string $ \s -> return (RpString s)

-- Create placeholder for a global function which will be later defined
createFun :: Stmt -> CodeGenModule (String, RpValue)
createFun (Assign name (Fun args _)) =
  case length args of
        0 -> do fun <- newNamedFunction ExternalLinkage name
                return $ (name, RpFun0 fun)
        1 -> do fun <- newNamedFunction ExternalLinkage name
                return $ (name, RpFun1 fun)
        2 -> do fun <- newNamedFunction ExternalLinkage name
                return $ (name, RpFun2 fun)

-- generate the functions found at the top level of the file
genModule :: Stmt -> CodeGenModule ()
genModule ast = do
  putchar <- newNamedFunction ExternalLinkage "putchar" :: TFunction (Int32 -> IO Int32)
  puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Int32)
  let strings = extractStrings ast
  cstrings <- sequence $ map intern strings
  funs <- sequence $ map createFun (extractFuns ast)
  let env = newEnv -$
            addStringLiterals (zip strings cstrings) -$
            envSetVar "putchar" (RpFun1 putchar) -$
            envSetVar "puts" (RpStrFun puts) -$
            envPushScope -$
            envSetVars funs -$
            envPushScope
  let genModule' ast =
        case ast of
          Seq stmts -> sequence_ $ map genModule' stmts
          Assign name (Fun args stmt) -> genFun name args stmt env
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

-- Extract the top level functions from an AST
extractFuns :: Stmt -> [Stmt]
extractFuns stmt =
    case stmt of
        Seq stmts     -> concat $ map extractFuns stmts
        Assign name e -> [stmt]
        _             -> []

-- write the bytecode for the given ast to a file
gen :: Stmt -> String -> IO ()
gen ast outputPath = do
  mod <- newModule
  defineModule mod (genModule ast)
  writeBitcodeToFile outputPath mod
