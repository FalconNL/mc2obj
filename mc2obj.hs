{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Data.List.Split
import System.Console.CmdArgs.Implicit
import System.Directory
import System.FilePath
import System.IO
import Text.Printf
import BlockDefs
import ObjExport
import Region

data Args = Args { objFolder :: FilePath, worldFolder :: FilePath
                 , minY :: Int, maxY :: Int
                 , rect :: Maybe (Int, Int, Int, Int), circle :: Maybe (Int, Int, Int)
                 , spawnRect :: Maybe (Int, Int), spawnCircle :: Maybe Int
                 , everything :: Bool, nether :: Bool, bottom :: Bool, sides :: Bool
                 } deriving (Show, Data, Typeable)

data ExportRegion = Circle Int Int Int | Rect Int Int Int Int | Everything

mc2obj :: Args
mc2obj = Args { objFolder    = "export" &= name "o"    &= help "The location of the .obj file" &= typFile
              , worldFolder  = ""       &= argPos 0 &= typ "WORLD_DIRECTORY"
              , minY         =       0                 &= help "minimum Y coordinate (bedrock = 0)"
              , maxY         =     127                 &= help "maximum Y coordinate (sealevel = 63)"
              , spawnRect    = Nothing                 &= help "rectangle centered on spawn: widthX,widthZ"
              , spawnCircle  = Nothing                 &= help "circle centered on spawn: radius"
              , rect         = Nothing                 &= help "rectangle: minX,minZ,maxX,maxZ"
              , circle       = Nothing                 &= help "circle: centerX,centerZ,radius"
              , everything   =   False  &= name "e"    &= help "export entire world"
              , nether       =   False  &= name "n"    &= help "output Nether instead of regular world"
              , bottom       =   False  &= name "b"    &= help "output bottom of world"
              , sides        =   False  &= name "s"    &= help "output sides of world"
              } &= summary version

version :: String
version = "mc2obj v0.6.5\nA program to convert Minecraft worlds to .obj files\nWritten by Remco Niemeijer (2011)"

main :: IO ()
main = do arguments <- E.onException (cmdArgs mc2obj) (hFlush stderr >> putStrLn "\nUse mc2obj -? for help information")
          (sx,sz) <- spawnChunk <$> loadNBT (worldFolder arguments </> "level.dat")
          let regionFolder = worldFolder arguments </> if nether arguments then "DIM-1/region" else "region"
          regions <- getDirectoryContents regionFolder
          let allChunks = concat [ [ (x,z) | x <- [rx*32..rx*32+31], z <- [rz*32..rz*32+31]]
                                 | region <- regions, let (filename, ext) = splitExtension region
                                 , ext == ".mcr", let (_:rx:rz:_) = map read $ splitOn "." filename]
          let exportRegion = maybe (Circle sx sz 0) id $ foldl1 (<|>)
                                 [Circle sx sz <$> spawnCircle arguments
                                 ,(\(w,h) -> Rect (sx-div w 2+1-mod w 2) (sz-div h 2+1-mod h 2)
                                                  (sx+div w 2) (sz+div h 2)) <$> spawnRect arguments
                                 ,(\(x,z,r) -> Circle x z r) <$> circle arguments
                                 ,(\(x1,z1,x2,z2) -> Rect x1 z1 x2 z2) <$> rect arguments
                                 ,(\e -> if e then Just Everything else Nothing) $ everything arguments]
          let chunks = case exportRegion of
                            (Rect x1 z1 x2 z2) -> [(x,z) | x <- [x1..x2], z <- [z1..z2]]
                            (Circle cx cz r)   -> [(x,z) | x <- [cx-r..cx+r], z <- [cz-r..cz+r]
                                                         ,fromIntegral ((x-cx)*(x-cx)) + fromIntegral ((z-cz)*(z-cz)) <
                                                          (fromIntegral r+1/2)*(fromIntegral r+1/2)]
                            _                  -> allChunks
          let worldName = takeFileName $ worldFolder arguments
          let texFolder = "texsplit/tex"
          createDirectoryIfMissing True $ objFolder arguments
          createDirectoryIfMissing True $ objFolder arguments </> "tex"
          copyFile "material/minecraft.mtl" (objFolder arguments </> "minecraft.mtl")
          mapM_ (\x -> copyFile x $ objFolder arguments </> "tex" </> takeFileName x) =<<
              filterM doesFileExist . map (texFolder </>) =<< getDirectoryContents "texsplit/tex"
          let suffix = case exportRegion of
                            (Rect x1 z1 x2 z2) -> printf "_r%d.%d.%d.%d" x1 z1 x2 z2
                            (Circle cx cz r)   -> printf "_c%d.%d.%d" cx cz r
                            _                  -> "_e"
          case chunks of
               [] -> putStrLn "Error: no region specified or incorrect region"
               cs -> export ExportOptions { showBottom = bottom arguments
                                          , showSides  = sides arguments
                                          , yFrom = minY arguments, yTo = maxY arguments }
                            blockDefs regionFolder
                            (objFolder arguments </> worldName ++ suffix <.> "obj") cs