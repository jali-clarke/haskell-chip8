{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeNatsHelpers
  ( addOne,
    addTwo,
    subOne,
  )
where

import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Type.Equality ((:~:) (..))
import qualified GHC.TypeLits.Compare as TypeNats
import GHC.TypeLits.Witnesses (SNat (..), (%+))
import GHC.TypeNats (type (+))
import qualified GHC.TypeNats as TypeNats

addOne :: forall n. TypeNats.KnownNat n => Finite n -> Maybe (Finite n)
addOne n =
  case (SNat :: SNat n) %+ (SNat :: SNat 2) of
    SNat ->
      case TypeNats.isLE (SNat :: SNat n) (SNat :: SNat (n + 2)) of
        Nothing -> Nothing
        Just Refl -> Finite.strengthenN $ Finite.add n one

addTwo :: forall n. TypeNats.KnownNat n => Finite n -> Maybe (Finite n)
addTwo n =
  case (SNat :: SNat n) %+ (SNat :: SNat 3) of
    SNat ->
      case TypeNats.isLE (SNat :: SNat n) (SNat :: SNat (n + 3)) of
        Nothing -> Nothing
        Just Refl -> Finite.strengthenN $ Finite.add n two

subOne :: Finite n -> Maybe (Finite n)
subOne n =
  case Finite.sub n one of
    Left _ -> Nothing
    Right nMinusOne -> Just nMinusOne

one :: Finite 2
one = Finite.finite 1

two :: Finite 3
two = Finite.finite 2
