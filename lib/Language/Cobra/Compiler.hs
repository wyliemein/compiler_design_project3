{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Cobra.Compiler ( compiler, compile ) where

import           Control.Arrow                   ((>>>))
import           Text.Printf                     (printf)
import           Prelude                  hiding (compare)
import           Data.Maybe
import           Data.Bits                       (shift)
import           Language.Cobra.Types      hiding (Tag)
import           Language.Cobra.Parser     (parse)
import           Language.Cobra.Normalizer (anormal)
import           Language.Cobra.Asm        (asm)


--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> anormal >>> tag >>> compile >>> asm

--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
type ABind = Bind    Tag

instance Located Tag where
  sourceSpan = fst

--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: Bare -> AExp
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
-- | @compile@ a (tagged-ANF) expr into assembly
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = funInstrs (countVars e) (compileEnv emptyEnv e)

-- | @funInstrs n body@ returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.
funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs = funEntry n ++ instrs ++ funExit

-- | TBD: insert instructions for setting up stack-frame for `n` local vars
funEntry :: Int -> [Instruction]
funEntry n  = [IPush (Reg EBP), IMov (Reg EBP) (Reg ESP), ISub (Reg ESP) (Const (4 * n))]

-- | TBD: cleaning up stack-frame after function finishes
funExit :: [Instruction]
funExit   = [IMov (Reg ESP) (Reg EBP), IPop (Reg EBP), IRet]

--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
--------------------------------------------------------------------------------
countVars (Let _ e b _)  = max (countVars e)  (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@Number {}       = [ compileImm env v  ]

compileEnv env v@Boolean {}      = [ compileImm env v  ]

compileEnv env v@Id {}           = [ compileImm env v  ]

compileEnv env e@Let {}          = is ++ compileEnv env' body
  where
    (env', is)                   = compileBinds env [] binds
    (binds, body)                = exprBinds e

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 l)    = compileIf l env v e1 e2

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
immArg _   (Boolean b _) = repr b
immArg env e@(Id x _)    = stackVar (fromMaybe err (lookupEnv x env))
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " ++ show (strip e)
    strip                = fmap (const ())

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l x = mkError (printf "Unbound variable %s" x) l

-- | TBD: Implement code for `Prim1` with appropriate type checking
compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env op v | pprint op == "add1" = assertType env v TNumber ++ compileEnv env v ++ [IAdd (Reg EAX) (Const 1)] 
                        | pprint op == "sub1" = assertType env v TNumber ++ compileEnv env v ++ [IAdd (Reg EAX) (Const 1)]
                        | pprint op == "print" = compileEnv env v ++ [IPush (Reg EAX), ICall (Builtin "print")]
                        | pprint op == "isNum" = assertType env v TNumber
                        | pprint op == "isBool" = assertType env v TBoolean

-- | TBD: Implement code for `Prim2` with appropriate type checking
compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 l env Plus v1 v2 = assertType env v1 TNumber ++ assertType env v2 TNumber ++ [IMov (Reg EAX) (immArg env v1), IAdd (Reg EAX) (immArg env v2)]
compilePrim2 l env Minus v1 v2 = assertType env v1 TNumber ++ assertType env v2 TNumber ++ [IMov (Reg EAX) (immArg env v1), ISub (Reg EAX) (immArg env v2)]
compilePrim2 l env Times v1 v2 = assertType env v1 TNumber ++ 
                                  assertType env v2 TNumber ++ 
                                  [IMov (Reg EAX) (immArg env v1), IMul (Reg EAX)( immArg env v2), ISar (Reg EAX) (Const 1)]
compilePrim2 l env _ _ _ = error "TBD "

-- | TBD: Implement code for `If` with appropriate type checking
compileIf :: Tag -> Env -> IExp -> AExp -> AExp -> [Instruction]
compileIf l env v e1 e2 = error "TBD:compileIf"

stackVar :: Int -> Arg
stackVar i = RegOffset (-4 * i) EBP

assertType :: Env -> IExp -> Ty -> [Instruction]
assertType env v ty = [IMov (Reg EAX) (immArg env v)
                      , IMov (Reg EBX) (Reg EAX)
                      , IAnd (Reg EBX) (Sized DWordPtr (typeMask ty))
                      , ICmp (Reg EBX) (Sized DWordPtr (typeTag ty))
                      , IJne (DynamicErr (TypeError ty))]

--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Bool where
  repr True  = HexConst 0xffffffff
  repr False = HexConst 0x7fffffff

instance Repr Int where
  repr n = Const (fromIntegral (shift n 1))

instance Repr Integer where
  repr n = Const (fromIntegral (shift n 1))

typeTag :: Ty -> Arg
typeTag TNumber   = HexConst 0x00000000
typeTag TBoolean  = HexConst 0x7fffffff

typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
