{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SizedByteString
  ( SizedByteString,
    withSized,
    byteAt,
    length',
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Finite (Finite)
import qualified Data.Finite as Finite
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import qualified GHC.TypeNats as TypeNats

newtype SizedByteString (n :: TypeNats.Nat) = SizedByteString ByteString

withSized :: ByteString -> (forall n. TypeNats.KnownNat n => SizedByteString n -> r) -> r
withSized unsizedByteString callback =
  case TypeNats.someNatVal (fromIntegral $ ByteString.length unsizedByteString) of
    TypeNats.SomeNat (_ :: Proxy n0) -> callback (SizedByteString unsizedByteString :: SizedByteString n0)

byteAt :: SizedByteString n -> Finite n -> Word8
byteAt (SizedByteString raw) index = ByteString.index raw (fromIntegral $ Finite.getFinite index)

length' :: TypeNats.KnownNat n => SizedByteString n -> Proxy n
length' _ = Proxy
