{-# LANGUAGE
    DeriveGeneric, DeriveAnyClass, DerivingStrategies,
    GeneralizedNewtypeDeriving
  #-}

import Data.HashCons
import Data.HashCons.Memo
import GHC.Generics

type Id = HC String


-- subterms are also hashconsed, so that fast equality is immediately available
-- while actually processing the AST
data Expr' =
    Var Id
  | Lam Id Expr
  | App Expr Expr
  deriving stock    (Eq, Generic)
  deriving anyclass (Hashable, HashCons)

type Expr = HC Expr'

subst :: Id -> Expr -> Expr -> Expr
subst x e s = subst' e s x

-- (arguments reordered for memo3's benefit)
subst' :: Expr -> Expr -> Id -> Expr
subst' = memo3 $ \e s x -> case getVal s of
  Var y   | x == y    -> e
          | otherwise -> s
  Lam y a | x == y    -> s
          | otherwise -> hc $ Lam y (subst' e a x)
  App a b             -> hc $ App (subst' e a x) (subst' e b x)


main :: IO ()
main = pure ()
