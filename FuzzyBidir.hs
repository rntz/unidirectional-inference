{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
module FuzzyBidir where

import Data.Monoid
import Control.Monad

data IsFuzzy = Fuzzy | Exact deriving (Show, Eq, Ord)

data Type :: IsFuzzy -> * where
  Wild :: Type Fuzzy
  -- An integer-indexed family of base types.
  Base :: Int -> Type a
  (:->) :: Type a -> Type a -> Type a

instance Show (Type a) where
    show Wild = "_"
    show (Base n) = show n
    show (a :-> b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

-- An exact type can always be regarded as fuzzy.
toFuzzy :: Type a -> Type Fuzzy
toFuzzy Wild = Wild
toFuzzy (Base n) = Base n
toFuzzy (a :-> b) = toFuzzy a :-> toFuzzy b

-- A fuzzy type is sometimes exact.
toExact :: Type Fuzzy -> Maybe (Type a)
toExact Wild = Nothing
toExact (Base n) = Just (Base n)
toExact (a :-> b) = liftM2 (:->) (toExact a) (toExact b)

-- Combines information from two fuzzy types.
merge :: Type Fuzzy -> Type Fuzzy -> Either String (Type Fuzzy)
merge Wild b = return b
merge a Wild = return a
merge (Base n) (Base m) | n == m = return (Base n)
merge (a1 :-> b1) (a2 :-> b2) = liftM2 (:->) (merge a1 a2) (merge b1 b2)
merge a b = Left ("cannot unify " ++ show a ++ " with " ++ show b)

-- Combines a fuzzy type with an exact one. Operationally the same as merge, but
-- Haskell isn't smart enough to notice it's a valid refinement.
check :: Type Exact -> Type Fuzzy -> Either String (Type Exact)
check a Wild = return a
check (Base n) (Base m) | n == m = return (Base n)
check (a1 :-> b1) (a2 :-> b2) = liftM2 (:->) (check a1 a2) (check b1 b2)
check a b = Left ("cannot unify " ++ show a ++ " with " ++ show b)


---------- Bidirectional type inference ----------
type Var = String
-- Variables are always associated with exact types.
type Env = [(Var, Type Exact)]

data Term
    = The (Type Fuzzy) Term -- type annotation
    | Var Var
    | Lambda Var Term
    | Call Term Term
      deriving (Show)

infer :: Env -> Type Fuzzy -> Term -> Either String (Type Exact)
infer env a (The b m) = do ab <- merge a b; infer env ab m
infer env a (Var v) =
    case lookup v env of
      Nothing -> Left $ "unbound variable " ++ show v
      Just exactType -> check exactType a
infer env (a :-> b) (Lambda x m) =
    case toExact a of
      Nothing -> Left $ "I can't type-check a lambda with a fuzzy argument type: "
                        ++ show a
      Just a -> liftM (a :->) $ infer ((x, a):env) b m
infer env Wild (Lambda x m) =
    Left $ "Don't know what type to give argument to function"
infer env typ (Lambda x m) =
    Left $ "A lambda cannot have non-function type " ++ show typ
infer env typ (Call m n) =
    do (a :-> b) <- infer env (Wild :-> typ) m
       infer env (toFuzzy a) n
       return b


-- Test cases.
test :: Term -> Either String (Type Exact)
test = infer [] Wild

idTerm = Lambda "x" (Var "x")
constTerm = Lambda "x" (Lambda "y" (Var "x"))

-- should fail, need to type-annotate function
case1 = idTerm

-- we can give an annotation that provides only the argument type, and it will
-- work.
case2 = The (Base 0 :-> Wild) idTerm

-- won't work if we provide only return type.
case3 = The (Wild :-> Base 0) idTerm

-- We can combine annotations, each of which has only partial information.
case4 :: Term
case4 = The (Base 0 :-> Wild) $
        The (Wild :-> (Base 1 :-> Wild)) $
        constTerm

testMe :: IO ()
testMe = do
  Left _ <- return (test case1)
  Right (Base 0 :-> Base 0) <- return (test case2)
  Left _ <- return (test case3)
  Right (Base 0 :-> (Base 1 :-> Base 0)) <- return (test case4)
  return ()
