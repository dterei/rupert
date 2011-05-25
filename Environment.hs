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

module Environment(
    RpValue(..)
  , Environment
  , newEnv
  , addStringLiterals
  , envLookupString
  , envGetString
  , envLookupVar
  , envGetVar
  , envSetVar
  , envSetVars
  , envPushScope
  , envPopScope
  , (-$)
  ) where

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

-- a scope maps from names to values
type Scope = Map.Map String RpValue

data Environment = Environment
  { stringLiterals :: Map.Map String RpValue -- (String to RpString)
  -- name resolution works on a stack of scopes with globals at the bottom
  , scopes :: [Scope]
  }

getStringLiterals :: Environment -> Map.Map String RpValue
getStringLiterals env = stringLiterals env

setStringLiterals :: Map.Map String RpValue -> Environment -> Environment
setStringLiterals stringLiterals env =
    Environment { stringLiterals=stringLiterals
                , scopes=(getScopes env)
                }

getScopes :: Environment -> [Scope]
getScopes env = scopes env

setScopes :: [Scope] -> Environment -> Environment
setScopes scopes env =
    Environment { stringLiterals=(getStringLiterals env)
                , scopes=scopes
                }


-- an empty environment
newEnv = Environment { stringLiterals=Map.empty
                     , scopes=[Map.empty]
                     }

-- add string literals to the environment
addStringLiterals :: [(String, RpValue)] -> Environment -> Environment
addStringLiterals literals env =
  let oldLiterals = getStringLiterals env
      newLiterals = Map.fromList literals
      merged = Map.union oldLiterals newLiterals
  in setStringLiterals merged env

-- lookup a string literal in the environment
envLookupString :: String -> Environment -> Maybe RpValue
envLookupString string env =
    let Environment {stringLiterals=strings} = env
    in Map.lookup string strings

-- lookup a string literal in the environment,
-- but throw an error if it's not found
envGetString :: String -> Environment -> RpValue
envGetString string env =
    case envLookupString string env of
        Just v -> v
        Nothing -> error $ "string \"" ++ string ++ "\" not found"

-- lookup a variable in the environment
envLookupVar :: String -> Environment -> Maybe RpValue
envLookupVar name env =
    let Environment {scopes=scopes} = env
        scopesLookupVar name scopes =
            case scopes of
                scope:rest -> case Map.lookup name scope of
                                Just v -> Just v
                                Nothing -> scopesLookupVar name rest
                []         -> Nothing
    in scopesLookupVar name scopes

-- lookup a name in the environment, but throw an error if it's not found
envGetVar :: String -> Environment -> RpValue
envGetVar name env =
    case envLookupVar name env of
        Just v -> v
        Nothing -> error $ "variable '" ++ name ++ "' not in scope"

-- set a name in the environment
-- If it exists, replace the old value.
-- Otherwise, create a new one at the innermost scope.
envSetVar :: String -> RpValue -> Environment -> Environment
envSetVar name value env =
  let Environment {scopes=scopes@(scope:rest)} = env
      scopeSet' name value scopes@(scope:rest) =
          case Map.lookup name scope of
            Just _ -> (Map.insert name value scope):rest
            Nothing -> scope:(scopeSet' name value rest)
      newScopes = case envLookupVar name env of
        Just _ -> scopeSet' name value scopes
        Nothing -> (Map.insert name value scope):rest
  in setScopes newScopes env

-- set names in the environment, see envSetVar
envSetVars :: [(String, RpValue)] -> Environment -> Environment
envSetVars [] env = env
envSetVars ((name, value):rest) env = env -$
                                      envSetVar name value -$
                                      envSetVars rest

-- Push a new scope onto the stack
envPushScope :: Environment -> Environment
envPushScope env =
  let oldScopes = getScopes env
  in setScopes (Map.empty : oldScopes) env

-- Pop a scope from the stack
envPopScope :: Environment -> Environment
envPopScope env =
  let scope:rest = getScopes env
  in setScopes rest env

-- flipped $, useful for chaining environment modifications
(-$) :: a -> (a -> b) -> b
x -$ f = f $ x
infixl 0  -$
