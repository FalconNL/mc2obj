{-# LANGUAGE OverloadedStrings #-}

import Data.Binary
import qualified Data.IntSet as S
import qualified Data.Vector as V
import ObjExport
import Region

chunkW, chunkH :: Int
(chunkW, chunkH) = (16, 128)

chunkData :: NBT -> (V.Vector Word8, V.Vector Word8)
chunkData nbt = (getArray $ navigate ["Level", "Blocks"] nbt,
                 getArray $ navigate ["Level", "Data"] nbt)
    where getArray ~(Just (TAG_Byte_Array a)) = a

blockGeometry :: V.Vector Word8 -> Location -> Word8 -> [Face]
blockGeometry _      _       0 = []
blockGeometry blocks (x,y,z) t = map (\(o,_) -> ((x,y,z), t, o)) $
    filter (maybe True (\t' -> t /= t' && S.notMember (fromIntegral t') solidIDs) .
        getBlock blocks . snd) [ (S, (x+1,y,z)), (N, (x-1,y,z)), (T, (x,y+1,z))
                               , (B, (x,y-1,z)), (W, (x,y,z+1)), (E, (x,y,z-1))]

locationFromIndex :: Int -> (Int, Int, Int)
locationFromIndex i = (x,y,z) where (x,r) = divMod i (chunkW * chunkH)
                                    (z,y) = divMod r chunkH

getBlock :: V.Vector Word8 -> Location -> Maybe Word8
getBlock blocks (x,y,z) = if x < 0 || x >= chunkW || z < 0 || z >= chunkW ||
    y < 0 || y >= chunkH then Nothing else blocks V.!? (y + z * chunkH + x * chunkH * chunkW)

solidIDs :: S.IntSet
solidIDs = S.fromAscList $ [1..5] ++ [7] ++ [11..17] ++ [19] ++ [21..25] ++
                           [29,33,35,36] ++ [41..43] ++ [45..49] ++ [54,56,57,58] ++
                           [60..62] ++ [73,74,78,79,80,82,84] ++ [86..89] ++ [91,95]

main :: IO ()
main = do r <- loadRegion "e:/mc2obj/regions/r.0.0.mcr"
          let blocks = fst . chunkData $ getChunk 0 0 r
          let faces = concat . V.toList $ V.imap (blockGeometry blocks . locationFromIndex) blocks
          export "e:/mc2obj/test.obj" faces