--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------

module Language.Boa.Normalizer ( anormal ) where

import           Language.Boa.Types

type Binds a = [(Bind a, (AnfExpr a, a))]

--------------------------------------------------------------------------------
-- | Convert an Expr into A-Normal Form
--------------------------------------------------------------------------------
anormal :: Expr a -> AnfExpr a
--------------------------------------------------------------------------------
anormal e = snd (anf 0 e)



-- Given function to study, not to use
-- lets :: [(Id, AnfE)] -> AnfE -> AnfE
-- lets [] e'         = e
-- lets ((x,e):bs) e' = Let x e (lets bs e')

--------------------------------------------------------------------------------
-- | `anf i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `e'` is equivalent to `e` but is in A-Normal Form.
--------------------------------------------------------------------------------
anf :: Int -> Expr a -> (Int, AnfExpr a)
--------------------------------------------------------------------------------
anf i (Number n l)      = (i, Number n l)

anf i (Id     x l)      = (i, Id     x l)


-- Should get immediate values from x, b, and bindings from both
anf i (Let x e b l)     =  ( i'', (Let x e' b' l))
  where
    (i' , e') = anf i e
    (i'', b') = anf i' b
-- error "TBD:anf:prim2"

anf i (Prim1 o e l)     = (i', stitch bs  (Prim1 o ae l))
-- stitch does the reverse of "lets"
  where
    (i', bs, ae)        = imm i e

-- Edited by self
anf i (Prim2 o e1 e2 l) = (i'', stitch (bs2 ++ bs1)
                            (Prim2 o e1' e2' l))
  where
    (i'   , bs1, e1')      = imm i  e1
    (i''  , bs2, e2')      = imm i' e2


anf i (If c e1 e2 l)    = (i''', stitch bs  (If c' e1' e2' l))
  where
    (i'  , bs, c')      = imm i   c
    (i'' ,     e1')     = anf i'  e1
    (i''',     e2')     = anf i'' e2

--------------------------------------------------------------------------------
-- | `stitch bs e` takes a "context" `bs` which is a list of temp-vars and their
--   definitions, and an expression `e` that uses the temp-vars in `bs` and glues
--   them together into a `Let` expression.
--   NOTE: the binders are in reverse order.
--------------------------------------------------------------------------------
stitch :: Binds a -> AnfExpr a -> AnfExpr a
--------------------------------------------------------------------------------
stitch bs e = bindsExpr [ (x, xe) | (x, (xe, _)) <- reverse bs] e (getLabel e)

--------------------------------------------------------------------------------
-- | `imms i es` takes as input a "start" counter `i` and expressions `es`, and
--   and returns an output `(i', bs, es')` where
--   * `i'` is the output counter (i.e. i'- i) anf-variables were generated
--   * `bs` are the temporary binders needed to convert `es` to immediate vals
--   * `es'` are the immediate values  equivalent to es
--------------------------------------------------------------------------------
imms :: Int -> [AnfExpr a] -> (Int, Binds a, [ImmExpr a])
--------------------------------------------------------------------------------
imms i []           = (i, [], [])
imms i (e:es)       = (i'', bs' ++ bs, e' : es' )
  where
    (i' , bs , e' ) = imm  i  e
    (i'', bs', es') = imms i' es

--------------------------------------------------------------------------------
-- | `imm i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', bs, e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `bs` are the temporary binders needed to render `e` in ANF, and
--   * `e'` is an `imm` value (Id or Number) equivalent to `e`.
--------------------------------------------------------------------------------
imm :: Int -> AnfExpr a -> (Int, Binds a, ImmExpr a)
--------------------------------------------------------------------------------
-- Base Cases, Id or Number, should have 0 binding
imm i (Number n l)      = (i, [], (Number n l))
-- immExp i (Number n l) l  -- Apparently doesn't return correct binding
imm i (Id x l)          = (i, [], (Id x l))
-- error "TBD:imm:prim2"
-- Good function to learn from --
imm i (Prim1 o e1 l)    = (i'', bs, mkId v l)
  where
    (i' , b1s, v1)      = imm i e1
    (i'', v)            = fresh l i'
    bs                  = (v, (Prim1 o v1 l, l)) : b1s -- Has to be this way **

-- !Error you did, you put b1s in front of b2s, when most recent binding is b2s
imm i (Prim2 o e1 e2 l) = ( i''' , bs , mkId v l   )
  where
    (i' , b1s, v1)      = imm i e1
    (i'' , b2s, v2)     = imm i' e2
    b3s                 = b2s++b1s
    (i''', v)           = fresh l i''
    bs                  = (v, (Prim2 o v1 v2 l, l)) : b3s -- Has to be this way **




    -- (i' , b1s, v1)      = imm i e1
    -- (i'', v)            = fresh l i'
    -- bs                  = (v, (Prim1 o v1 l, l)) : b1s -- Has to be this way **
    -- (i''' , b2s, v2)    = imm i e1
    -- (i'''', v')         = fresh l i'
    -- bs'                 = (v', (Prim1 o v2 l, l)) : b2s -- Has to be this way **
    -- bss                 = bs'++bs



imm i e@(If _ _ _  l)   = immExp i e l

imm i e@(Let _ _ _ l)   = immExp i e l


immExp :: Int -> AnfExpr a -> a -> (Int, Binds a, ImmExpr a)
immExp i e l  = (i'', bs, mkId v l)
  where
    (i' , e') = anf i e
    (i'', v)  = fresh l i'
    bs        = [(v, (e', l))]

mkId :: Bind a -> a -> Expr a
mkId x l = Id (bindId x) l

--------------------------------------------------------------------------------
-- | `fresh i` returns a temp-var named `i` and "increments" the counter
--------------------------------------------------------------------------------
fresh :: a -> Int -> (Int, Bind a)
--------------------------------------------------------------------------------
fresh l i = (i + 1, Bind x l)
  where
    x     = "anf" ++ show i
