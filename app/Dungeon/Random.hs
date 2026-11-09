module Dungeon.Random (hash, chance) where

import Data.Bits (shiftR, xor, (.&.))

hash :: Int -> Int -> Int -> Int
hash a b c = h3 .&. 0x7fffffff
  where
    h0 = (a * 73856093) `xor` (b * 19349663) `xor` (c * 83492791)
    h1 = (h0 `xor` (h0 `shiftR` 13)) * 1274126177
    h2 = h1 `xor` (h1 `shiftR` 16)
    h3 = h2 * 668265263 `xor` (h2 `shiftR` 11)

chance :: Int -> Int -> Int -> Int -> Bool
chance percent a b c = hash a b c `mod` 100 < percent
