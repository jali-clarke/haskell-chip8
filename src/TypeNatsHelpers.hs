{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeNatsHelpers where

import Data.Finite (Finite)
import qualified Data.Finite as Finite
import GHC.TypeNats (type (+), type (<=))
import qualified GHC.TypeNats as TypeNats

addOne :: (TypeNats.KnownNat n, n <= n + 2) => Finite n -> Maybe (Finite n)
addOne n = Finite.strengthenN $ Finite.add n one

addTwo :: (TypeNats.KnownNat n, n <= n + 3) => Finite n -> Maybe (Finite n)
addTwo n = Finite.strengthenN $ Finite.add n two

one :: Finite 2
one = Finite.finite 1

two :: Finite 3
two = Finite.finite 2
