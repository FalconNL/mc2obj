module Region (Tag(..), loadRegion, navigate) where

--import qualified Codec.Compression.GZip as G
import Codec.Compression.Zlib
import Control.Applicative
import Control.Monad
import Data.Binary.Strict.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 ()
import qualified Data.IntMap as I
import Data.List
import Data.Word

data Tag = TAG_End | TAG_Byte Word8 | TAG_Short Word16 | TAG_Int Word32
         | TAG_Long Word64 | TAG_Float Float | TAG_Double Double
         | TAG_Byte_Array B.ByteString | TAG_String B.ByteString
         | TAG_List [Tag] | TAG_Compound [(B.ByteString, Tag)] deriving Show

getNBT :: Get Tag
getNBT = byte >> string >> tag 10

getRegion :: Get (I.IntMap Tag)
getRegion = do cLengths <- (\y -> zipWith (\((o1,_),_) ((o2,_),i) -> (i, o1 - o2))
                                          (tail y ++ [(\((o,l),i) -> ((o+l,l),i)) $ last y]) y) .
                           sort . filter ((> 0) . fst . fst) . flip zip [0..] . map (flip divMod 256) <$> 
                           replicateM 1024 int <* replicateM 1024 int
               I.fromList <$> mapM (\(i, y) -> ((,) i) . run getNBT <$> chunk y) cLengths

tag :: Word8 -> Get Tag
tag  0 = return TAG_End
tag  1 = TAG_Byte       <$> byte
tag  2 = TAG_Short      <$> short
tag  3 = TAG_Int        <$> int
tag  4 = TAG_Long       <$> getWord64be
tag  5 = TAG_Float      <$> getFloat32host
tag  6 = TAG_Double     <$> getFloat64host
tag  7 = TAG_Byte_Array <$> (getByteString . fromIntegral =<< int)
tag  8 = TAG_String     <$> string
tag  9 = TAG_List       <$> join ((tag <$> byte) <**> (replicateM . fromIntegral <$> int))
tag 10 = TAG_Compound   <$> compound
tag  n = error $ "Unrecognized tag type: " ++ show n

byte = getWord8
short = getWord16be
int = getWord32be
string = (getByteString . fromIntegral =<< short)
compound = byte >>= \tagType -> if tagType == 0 then return [] else 
    (:) <$> ((,) <$> string <*> tag tagType) <*> compound

chunk :: Integral a => a -> Get B.ByteString
chunk len = int *> byte *> (B.concat . BL.toChunks . decompress . BL.fromChunks . return <$>
                                (getByteString . fromIntegral $ len * 4096 - 5))

loadRegion :: FilePath -> IO (I.IntMap Tag)
loadRegion file = run getRegion <$> B.readFile file

run :: Get a -> B.ByteString -> a
run p = either error id . fst . runGet p

--loadNBT :: FilePath -> IO Tag
--loadNBT file = run getNBT . B.concat . BL.toChunks . G.decompress <$> BL.readFile file

navigate :: [B.ByteString] -> Tag -> Maybe Tag
navigate xs ct = foldl' (\a x -> ref x =<< a) (Just ct) xs where
    ref k (TAG_Compound ts) = lookup k ts
    ref _ _                 = Nothing