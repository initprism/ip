{-# LANGUAGE TypeApplications #-}

module Lookup.ParseIP where

import Data.Word
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Bits (toIntegralSized, shiftL)
import Data.List.Split (splitOn)
import Text.Read
import Control.Applicative
import Control.Monad
import Lookup.IPTypes

buildIP :: [Word8] -> IP
buildIP = buildIP_foldl_shl
    
{-# INLINE buildIP_foldr #-}
buildIP_foldr :: [Word8] -> IP
buildIP_foldr = IP . fst . foldr go (0, 1)
    where 
        go b (s, k) = (s + fromIntegral b * k, k * 256)

{-# INLINE buildIP_foldl #-}
buildIP_foldl :: [Word8] -> IP
buildIP_foldl = IP . foldl (\s b -> s * 256 + fromIntegral b) 0

{-# INLINE buildIP_foldl_shl #-}
buildIP_foldl_shl :: [Word8] -> IP
buildIP_foldl_shl = IP .foldl (\s b -> shiftL s 8 + fromIntegral b) 0

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f a = if f a then pure a else empty

isLengthOf :: Int -> [a] -> Bool
isLengthOf n xs = length xs == n

parseIP :: String -> Maybe IP
parseIP = guarded (4 `isLengthOf` ) . splitOn "."
            >=> mapM  (readMaybe @Integer >=> toIntegralSized)
            >=> pure . buildIP
    where 
        fitsOctet x = 0 <= x && x <= 255

parseIPRange :: String -> Maybe IPRange
parseIPRange = guarded (2 `isLengthOf`) . splitOn ","
                >=> mapM parseIP 
                >=> listToIPRange
    where 
        listToIPRange [a, b] | a <= b = pure (IPRange a b)
        listToIPRange _ = empty

parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges = fmap IPRangeDB . mapM parseLine . zip [1..] . lines
    where 
        parseLine (ln, s) = 
                case parseIPRange s of
                    Nothing  -> Left (ParseError ln)
                    Just ipr -> Right ipr

parseValidIPs :: String -> [IP]
parseValidIPs = mapMaybe parseIP . lines

parseValidIPRanges :: String -> IPRangeDB
parseValidIPRanges = IPRangeDB . mapMaybe parseIPRange . lines
