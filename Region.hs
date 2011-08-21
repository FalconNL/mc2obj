module Region (Region(..), NBT(..), Tag(..),
       getChunk, loadNBT, loadRegion, navigate) where

import qualified Codec.Compression.GZip as G
import Codec.Compression.Zlib
import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 ()
import Data.Int
import qualified Data.IntMap as I
import Data.List
import qualified Data.Vector as V

data Region = Region (I.IntMap NBT) deriving Show
data NBT = NBT Tag deriving Show
data Tag = TAG_End | TAG_Byte Word8 | TAG_Short Int16 | TAG_Int Int32
         | TAG_Long Int64 | TAG_Float Float | TAG_Double Double
         | TAG_Byte_Array (V.Vector Word8) | TAG_String B.ByteString
         | TAG_List [Tag] | TAG_Compound [(B.ByteString, Tag)] deriving Show

instance Binary NBT where
    put = undefined
    get = NBT <$> (byte >> string >> tag 10)

instance Binary Region where
    put = undefined
    get = do cLengths <- (\y -> zipWith (-) (map fst (tail y) ++ [uncurry (+) $ last y]) (map fst y)) .
                         sort . filter ((> 0) . fst) . map (flip divMod 256) <$> 
                         replicateM 1024 int <* replicateM 1024 int
             Region . I.fromList . zip [0..] <$> mapM ((decode <$>) . chunk) cLengths

tag :: Word8 -> Get Tag
tag  0 = return TAG_End
tag  1 = TAG_Byte <$> byte
tag  2 = TAG_Short <$> short
tag  3 = TAG_Int <$> int
tag  4 = TAG_Long <$> get
tag  5 = TAG_Float <$> getFloat32be
tag  6 = TAG_Double <$> getFloat64be
tag  7 = TAG_Byte_Array . V.fromList <$> (bytes =<< int)
tag  8 = TAG_String <$> string
tag  9 = TAG_List <$> join ((tag <$> byte) <**> (replicateM . fromIntegral <$> int))
tag 10 = TAG_Compound <$> compound
tag  n = error $ "Unrecognized tag type: " ++ show n

byte = get :: Get Word8
short = get :: Get Int16
int = get :: Get Int32
string = B.pack <$> (bytes =<< short)
compound = byte >>= \tagType -> if tagType == 0 then return [] else 
    (:) <$> ((,) <$> string <*> tag tagType) <*> compound

bytes :: Integral a => a -> Get [Word8]
bytes = flip replicateM byte . fromIntegral
chunk len = int *> byte *> (decompress . B.pack <$> (bytes $ len * 4096 - 5))

loadRegion :: FilePath -> IO Region
loadRegion = decodeFile

loadNBT :: FilePath -> IO NBT
loadNBT file = decode . G.decompress <$> B.readFile file

navigate :: [B.ByteString] -> NBT -> Maybe Tag
navigate xs (NBT ct) = foldl (\a x -> ref x =<< a) (Just ct) xs where
    ref k (TAG_Compound ts) = lookup k ts
    ref _ _                 = Nothing

getChunk :: Int -> Int -> Region -> NBT
getChunk x z (Region cs) = cs I.! (mod x 32 + 32 * mod z 32)