{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module VM.Sprites
  ( allSprites,
  )
where

import qualified Data.Vector.Unboxed.Sized as SizedVector
import Data.Word (Word8)
import GHC.TypeNats (type (*))

type BytesPerSprite = 5

type NumSprites = 16

allSprites :: SizedVector.Vector (NumSprites * BytesPerSprite) Word8
allSprites =
  let allSpritesList =
        concat
          [ sprite0,
            sprite1,
            sprite2,
            sprite3,
            sprite4,
            sprite5,
            sprite6,
            sprite7,
            sprite8,
            sprite9,
            spritea,
            spriteb,
            spritec,
            sprited,
            spritee,
            spritef
          ]
   in case SizedVector.fromList allSpritesList of
        Nothing -> error "the author of this code failed to size VM.Sprites.allSprites correctly"
        Just allSpritesVec -> allSpritesVec

sprite0 :: [Word8]
sprite0 =
  [ 0b00111100,
    0b00100100,
    0b00100100,
    0b00100100,
    0b00111100
  ]

sprite1 :: [Word8]
sprite1 =
  [ 0b00011000,
    0b00011000,
    0b00011000,
    0b00011000,
    0b00011000
  ]

sprite2 :: [Word8]
sprite2 =
  [ 0b00111100,
    0b00000100,
    0b00111100,
    0b00100000,
    0b00111100
  ]

sprite3 :: [Word8]
sprite3 =
  [ 0b00111100,
    0b00000100,
    0b00111100,
    0b00000100,
    0b00111100
  ]

sprite4 :: [Word8]
sprite4 =
  [ 0b00100100,
    0b00100100,
    0b00111100,
    0b00000100,
    0b00000100
  ]

sprite5 :: [Word8]
sprite5 =
  [ 0b00111100,
    0b00100000,
    0b00111100,
    0b00000100,
    0b00111100
  ]

sprite6 :: [Word8]
sprite6 =
  [ 0b00111100,
    0b00100000,
    0b00111100,
    0b00100100,
    0b00111100
  ]

sprite7 :: [Word8]
sprite7 =
  [ 0b00111100,
    0b00000100,
    0b00000100,
    0b00000100,
    0b00000100
  ]

sprite8 :: [Word8]
sprite8 =
  [ 0b00111100,
    0b00100100,
    0b00111100,
    0b00100100,
    0b00111100
  ]

sprite9 :: [Word8]
sprite9 =
  [ 0b00111100,
    0b00100100,
    0b00111100,
    0b00000100,
    0b00111100
  ]

spritea :: [Word8]
spritea =
  [ 0b00111100,
    0b00100100,
    0b00111100,
    0b00100100,
    0b00100100
  ]

spriteb :: [Word8]
spriteb =
  [ 0b00111100,
    0b00010100,
    0b00011100,
    0b00010100,
    0b00111100
  ]

spritec :: [Word8]
spritec =
  [ 0b00111100,
    0b00100000,
    0b00100000,
    0b00100000,
    0b00111100
  ]

sprited :: [Word8]
sprited =
  [ 0b00111100,
    0b00010100,
    0b00010100,
    0b00010100,
    0b00111100
  ]

spritee :: [Word8]
spritee =
  [ 0b00111100,
    0b00100000,
    0b00111100,
    0b00100000,
    0b00111100
  ]

spritef :: [Word8]
spritef =
  [ 0b00111100,
    0b00100000,
    0b00111100,
    0b00100000,
    0b00100000
  ]
