{-# LANGUAGE FlexibleInstances #-}

module Data.Interpolate (
      Interpolate(..)
    ) where


import Data.Function
import Data.Ratio
import Data.Word


class Interpolate a where
    interpolate :: a -> a -> Rational -> a


instance Interpolate (Ratio Integer) where
    interpolate x y k = x + k * (y - x)


interpAsRational :: Integral a => a -> a -> Rational -> Rational
interpAsRational = interpolate `on` fromIntegral


interpByRational :: Integral a => a -> a -> Rational -> a
interpByRational x y = round . interpAsRational x y


instance Interpolate Int where
    interpolate = interpByRational


instance Interpolate Integer where
    interpolate = interpByRational


instance Interpolate Word8 where
    interpolate = interpByRational


instance Interpolate Word where
    interpolate = interpByRational


