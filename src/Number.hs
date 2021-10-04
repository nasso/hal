module Number
  ( Number (..),
    blur,
  )
where

import Data.Ratio (denominator, numerator)

data Number
  = Exact Integer
  | Ratio Rational
  | Inexact Double
  deriving (Eq)

instance Show Number where
  show (Exact i) = show i
  show (Ratio r) = show (numerator r) ++ "/" ++ show (denominator r)
  show (Inexact d) = show d

instance Num Number where
  Exact a + Exact b = Exact (a + b)
  Exact a + Ratio b = Ratio (fromIntegral a + b)
  Exact a + Inexact b = Inexact (fromIntegral a + b)
  Ratio a + Exact b = Ratio (a + fromIntegral b)
  Ratio a + Ratio b = Ratio (a + b)
  Ratio a + Inexact b = Inexact (fromRational a + b)
  Inexact a + Exact b = Inexact (a + fromIntegral b)
  Inexact a + Ratio b = Inexact (a + fromRational b)
  Inexact a + Inexact b = Inexact (a + b)
  Exact a * Exact b = Exact (a * b)
  Exact a * Ratio b = Ratio (fromIntegral a * b)
  Exact a * Inexact b = Inexact (fromIntegral a * b)
  Ratio a * Exact b = Ratio (a * fromIntegral b)
  Ratio a * Ratio b = Ratio (a * b)
  Ratio a * Inexact b = Inexact (fromRational a * b)
  Inexact a * Exact b = Inexact (a * fromIntegral b)
  Inexact a * Ratio b = Inexact (a * fromRational b)
  Inexact a * Inexact b = Inexact (a * b)
  negate (Exact a) = Exact (negate a)
  negate (Ratio a) = Ratio (negate a)
  negate (Inexact a) = Inexact (negate a)
  abs (Exact a) = Exact (abs a)
  abs (Ratio a) = Ratio (abs a)
  abs (Inexact a) = Inexact (abs a)
  signum (Exact a) = Exact (signum a)
  signum (Ratio a) = Ratio (signum a)
  signum (Inexact a) = Inexact (signum a)
  fromInteger a = Exact a

instance Fractional Number where
  fromRational a = Ratio a
  recip (Exact a) = Ratio (recip (fromIntegral a))
  recip (Ratio a) = Ratio (recip a)
  recip (Inexact a) = Inexact (recip a)
  Exact a / Exact b = Ratio (fromIntegral a / fromIntegral b)
  Exact a / Ratio b = Ratio (fromIntegral a / b)
  Exact a / Inexact b = Inexact (fromIntegral a / b)
  Ratio a / Exact b = Ratio (a / fromIntegral b)
  Ratio a / Ratio b = Ratio (a / b)
  Ratio a / Inexact b = Inexact (fromRational a / b)
  Inexact a / Exact b = Inexact (a / fromIntegral b)
  Inexact a / Ratio b = Inexact (a / fromRational b)
  Inexact a / Inexact b = Inexact (a / b)

instance Ord Number where
  compare (Exact a) (Exact b) = compare a b
  compare (Exact a) (Ratio b) = compare (fromIntegral a) b
  compare (Exact a) (Inexact b) = compare (fromIntegral a) b
  compare (Ratio a) (Exact b) = compare a (fromIntegral b)
  compare (Ratio a) (Ratio b) = compare a b
  compare (Ratio a) (Inexact b) = compare (fromRational a) b
  compare (Inexact a) (Exact b) = compare a (fromIntegral b)
  compare (Inexact a) (Ratio b) = compare a (fromRational b)
  compare (Inexact a) (Inexact b) = compare a b

instance Floating Number where
  pi = Inexact pi
  exp (Exact a) = Inexact (exp (fromIntegral a))
  exp (Ratio a) = Inexact (exp (fromRational a))
  exp (Inexact a) = Inexact (exp a)
  log (Exact a) = Inexact (log (fromIntegral a))
  log (Ratio a) = Inexact (log (fromRational a))
  log (Inexact a) = Inexact (log a)
  sin (Exact a) = Inexact (sin (fromIntegral a))
  sin (Ratio a) = Inexact (sin (fromRational a))
  sin (Inexact a) = Inexact (sin a)
  cos (Exact a) = Inexact (cos (fromIntegral a))
  cos (Ratio a) = Inexact (cos (fromRational a))
  cos (Inexact a) = Inexact (cos a)
  asin (Exact a) = Inexact (asin (fromIntegral a))
  asin (Ratio a) = Inexact (asin (fromRational a))
  asin (Inexact a) = Inexact (asin a)
  acos (Exact a) = Inexact (acos (fromIntegral a))
  acos (Ratio a) = Inexact (acos (fromRational a))
  acos (Inexact a) = Inexact (acos a)
  atan (Exact a) = Inexact (atan (fromIntegral a))
  atan (Ratio a) = Inexact (atan (fromRational a))
  atan (Inexact a) = Inexact (atan a)
  sinh (Exact a) = Inexact (sinh (fromIntegral a))
  sinh (Ratio a) = Inexact (sinh (fromRational a))
  sinh (Inexact a) = Inexact (sinh a)
  cosh (Exact a) = Inexact (cosh (fromIntegral a))
  cosh (Ratio a) = Inexact (cosh (fromRational a))
  cosh (Inexact a) = Inexact (cosh a)
  asinh (Exact a) = Inexact (asinh (fromIntegral a))
  asinh (Ratio a) = Inexact (asinh (fromRational a))
  asinh (Inexact a) = Inexact (asinh a)
  acosh (Exact a) = Inexact (acosh (fromIntegral a))
  acosh (Ratio a) = Inexact (acosh (fromRational a))
  acosh (Inexact a) = Inexact (acosh a)
  atanh (Exact a) = Inexact (atanh (fromIntegral a))
  atanh (Ratio a) = Inexact (atanh (fromRational a))
  atanh (Inexact a) = Inexact (atanh a)

blur :: Number -> Number
blur (Exact a) = Inexact (fromIntegral a)
blur (Ratio a) = Inexact (fromRational a)
blur (Inexact a) = Inexact a
