module Insieme.Arithmetic where

import Data.List hiding (product)
import Data.Ord
import Prelude hiding (exponent, product)

--
-- * Formula
--

-- | Something like @-2 * x^2 * y^3 + 5 * z^1@ where 'c' denotes the type
-- of coefficients and exponents, and 'v' the type of variables.
data Formula c v = Formula { terms :: [Term c v] }
  deriving (Eq)

instance (Eq c, Ord v) => Ord (Formula c v) where
    compare = comparing terms

instance Functor (Formula c) where
    fmap f = Formula . map (fmap f) . terms

instance (Integral c, Show c, Show v) => Show (Formula c v) where
    show = prettyShow

convert :: (Integral c, Integral c', Ord v) => Formula c v -> Formula c' v
convert = Formula . map convertTerm . terms

normalize :: (Integral c, Ord v) => Formula c v -> Formula c v
normalize = Formula . filter ((/=0) . coeff) . map merge
                    . groupBy (\x y -> product x == product y)
                    . sort . map normalizeTerm . terms
  where
    merge ts = Term (sum $ coeff <$> ts) (product . head $ ts)
    ensureZero [] = [Term 0 (Product[])]
    ensureZero xs = xs

prettyShow :: (Integral c, Show c, Show v) => Formula c v -> String
prettyShow (Formula ts) = intercalate " + " $ prettyShowTerm <$> ts

isConst :: Formula c v -> Bool
isConst = all termIsConst . terms

zero :: (Integral c, Ord v) => Formula c v
zero = Formula []

one :: (Integral c, Ord v) => Formula c v
one = Formula [Term 1 (Product [])]

mkConst :: (Integral c, Ord v) => c -> Formula c v
mkConst 0 = zero
mkConst c = Formula [Term c (Product [])]

mkVar :: (Integral c, Ord v) => v -> Formula c v
mkVar v = Formula [Term 1 (Product [Factor v 1])]


--
-- * Parts of a Formula
--

-- | Something like @x^2@
data Factor c v = Factor { base     :: v,
                           exponent :: c }
  deriving (Show, Eq)

instance (Eq c, Ord v) => Ord (Factor c v) where
    compare = comparing base

instance Functor (Factor c) where
    fmap f (Factor b e) = Factor (f b) e

convertFactor :: (Integral c, Integral c', Ord v) => Factor c v -> Factor c' v
convertFactor (Factor b e) = Factor b (fromIntegral e)

prettyShowFactor :: (Show c, Show v) => Factor c v -> String
prettyShowFactor (Factor b e) = show b ++ "^" ++ show e

-- | Something like @x^2 * y^3@
data Product c v = Product { factors :: [Factor c v] }
  deriving (Show, Eq)

instance (Eq c, Ord v) => Ord (Product c v) where
    compare = comparing factors

instance Functor (Product c) where
    fmap f = Product . map (fmap f) . factors

convertProduct :: (Integral c, Integral c', Ord v)
               => Product c v -> Product c' v
convertProduct = Product . map convertFactor . factors

normalizeProduct :: (Integral c, Ord v) => Product c v -> Product c v
normalizeProduct = Product . filter ((/=0) . exponent) . map merge
                           . groupBy (\x y -> base x == base y)
                           . sort . factors
  where
    merge fs = Factor (base . head $ fs) (sum $ exponent <$> fs)

prettyShowProduct :: (Show c, Show v) => Product c v -> String
prettyShowProduct = intercalate " " . map prettyShowFactor . factors

-- | Something like @-2 * x^2 * y^3@
data Term c v = Term { coeff   :: c,
                       product :: Product c v}
  deriving (Show, Eq)

instance (Eq c, Ord v) => Ord (Term c v) where
    compare = comparing product

instance Functor (Term c) where
    fmap f (Term c ps) = Term c (fmap f ps)

convertTerm :: (Integral c, Integral c', Ord v) => Term c v -> Term c' v
convertTerm (Term c p) = Term (fromIntegral c) (convertProduct p)

normalizeTerm :: (Integral c, Ord v) => Term c v -> Term c v
normalizeTerm (Term c p) = Term c (normalizeProduct p)

prettyShowTerm :: (Integral c, Show c, Show v) => Term c v -> String
prettyShowTerm (Term c p) = show c ++ " " ++ prettyShowProduct p

termIsConst :: Term c v -> Bool
termIsConst = null . factors . product

scaleTerm :: (Integral c) => c -> Term c v -> Term c v
scaleTerm s (Term c p) = Term (s * c) p


--
-- * Operations
--

addFormula :: (Integral c, Ord v) => Formula c v-> Formula c v -> Formula c v
addFormula a b = normalize $ addFormula' a b

subFormula :: (Integral c, Ord v) => Formula c v-> Formula c v -> Formula c v
subFormula a b = normalize $ addFormula' a (scaleFormula' (-1) b)

sumFormula :: (Integral c, Ord v) => [Formula c v] -> Formula c v
sumFormula = normalize . foldr1 addFormula'

scaleFormula :: (Integral c, Ord v) => c -> Formula c v -> Formula c v
scaleFormula s = normalize . scaleFormula' s

mulFormula :: (Integral c, Ord v) => Formula c v -> Formula c v -> Formula c v
mulFormula a b = normalize $ mulFormula' a b

productFormula :: (Integral c, Ord v) => [Formula c v] -> Formula c v
productFormula = normalize . foldr1 mulFormula'


--
-- * Utility
--

addFormula' :: (Integral c) => Formula c v-> Formula c v -> Formula c v
addFormula' (Formula as) (Formula bs) = Formula (as ++ bs)

scaleFormula' :: (Integral c) => c -> Formula c v -> Formula c v
scaleFormula' s = Formula . map (scaleTerm s) . terms

mulFormula' :: (Integral c) => Formula c v -> Formula c v -> Formula c v
mulFormula' (Formula as) (Formula bs) = Formula . concat $ terms'
  where
    terms' = mulTerms <$> as <*> pure bs

mulTerm :: (Integral c) => Term c v -> Term c v -> Term c v
mulTerm (Term a b) (Term c d) = Term (a * c) (Product $ factors b ++ factors d)

mulTerms :: (Integral c) => Term c v -> [Term c v] -> [Term c v]
mulTerms t = map (mulTerm t)


--
-- * Numeric Order
--

data NumOrdering = NumLT | NumEQ | NumGT | Sometimes
  deriving (Eq, Ord, Enum, Read, Show)

class NumOrd a where
    numCompare :: a -> a -> NumOrdering

fromOrdering :: Ordering -> NumOrdering
fromOrdering LT = NumLT
fromOrdering EQ = NumEQ
fromOrdering GT = NumGT

instance (Integral c, Ord v) => NumOrd (Formula c v) where
    numCompare a b =
        if diff == zero
            then NumEQ
            else if not (isConst diff)
                then Sometimes
                else fromOrdering $ compare (coeff . head . terms $ diff) 0
      where
        diff = addFormula a (scaleFormula' (-1) b)


--
-- * Examples
--

fa = Factor 'a' 2
fb = Factor 'b' 2
fx = Factor 'x' 2
fy = Factor 'y' 3
fz = Factor 'z' 1
t1 = Term 2 (Product [fx])
t2 = Term 5 (Product [fy])
t3 = Term 2 (Product [fz])
t4 = Term (-1) (Product [fa])
t5 = Term 4 (Product [fb])
t6 = Term 7 (Product [fx])
f1 = Formula [t1, t2, t3]
f2 = Formula [t4, t5, t6]
