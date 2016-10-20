{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Boa.Compiler ( compiler, compile ) where

import           Text.Printf                     (printf)
import           Prelude                 hiding (compare)
import           Control.Arrow           ((>>>))
import           Control.Monad           (void)
import           Data.Maybe
import           Language.Boa.Types      hiding (Tag)
import           Language.Boa.Parser     (parse)
import           Language.Boa.Normalizer (anormal)
import           Language.Boa.Asm        (asm)

--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> anormal >>> tag >>> compile >>> asm

-- | to test your compiler with code that is ALREADY in ANF comment out
--   the above definition and instead use the below:

-- compiler f = parse f >>> tag >>> compile >>> asm


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
type ABind = Bind    Tag

instance Located Tag where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------

-- Original code
tag :: Bare -> AExp
--------------------------------------------------------------------------------
tag = label




-- doTag :: Int -> Bare -> (Int, Tag)
-- doTag i (Number n _)    = (i + 1 , Number n i)
-- doTag i (Let x e1 e2 _) = (_2    , Let x e1' e2' i2)
--   where
--     (i1, e1')           = doTag i  e1
--     (i2, e2')           = doTag _1 e2




--------------------------------------------------------------------------------
-- | @compile@ a (tagged-ANF) expr into assembly
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = compileEnv emptyEnv e ++ [IRet]


--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@(Number {})     = [ compileImm env v  ]

compileEnv env v@(Id {})         = [ compileImm env v  ]

compileEnv env e@(Let {})        = is ++ compileEnv env' body
  where
    (env', is)                   = compileBinds env [] binds
    (binds, body)                = exprBinds e

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

-- compileEnv env (If v e1 e2 l)    =
compileEnv env (If eCond eThen eElse l)    =
  compileEnv env eCond
  ++ [ICmp (Reg EAX) (Const 0)
     ,IJne (BranchTrue (snd l))
     ]
  ++    compileEnv env eElse
  ++ [IJmp (BranchDone (snd l))
     ,ILabel (BranchTrue (snd l))
     ]
  ++    compileEnv env eThen
  ++ [ ILabel (BranchDone (snd l)) ]

-- error "TBD:compileEnv:If"

compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)

compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []     = (env, is)
compileBinds env is (b:bs) = compileBinds env' (is ++ is') bs
  where
    (env', is')            = compileBind env b

compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
  where
    is                 = compileEnv env e
                      ++ [IMov (stackVar i) (Reg EAX)]
    (i, env')          = pushEnv x env

immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg env e@(Id x _)    = case (lookupEnv  x env) of
                           Nothing -> err -- Why error?
                           Just i -> stackVar i -- Why stackVar, because it --> Gets you your register!

  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " ++ show (void e)

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l

--------------------------------------------------------------------------------
-- | Compiling Primitive Operations
--------------------------------------------------------------------------------
compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1 v = [ IMov (Reg EAX) (immArg env v)
                                  , IAdd (Reg EAX) (Const 1)
                                  ]
compilePrim1 l env Sub1 v =  [ IMov (Reg EAX) (immArg env v)
                                  , ISub (Reg EAX) (Const 1)
                                  ]

compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 l env Plus  v1 v2 = [ IMov (Reg EAX) (immArg env v1)
                                 , IAdd (Reg EAX) (immArg env v2)
                                 ]

compilePrim2 l env Minus v1 v2 = [ IMov (Reg EAX) (immArg env v1)
                                 , ISub (Reg EAX) (immArg env v2)
                                 ]
compilePrim2 l env Times v1 v2 = [ IMov (Reg EAX) (immArg env v1)
                                 , IMul (Reg EAX) (immArg env v2)
                                 ]

--------------------------------------------------------------------------------
-- | Local Variables
--------------------------------------------------------------------------------
stackVar :: Int -> Arg
--------------------------------------------------------------------------------
stackVar i = RegOffset (-4 * i) ESP

--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Int where
  repr n = Const (fromIntegral n)

instance Repr Integer where
  repr n = Const (fromIntegral n)
