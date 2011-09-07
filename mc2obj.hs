{-# LANGUAGE DeriveDataTypeable #-}

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

data Args = Args { objFolder :: FilePath, worldFolder :: FilePath
                 , minY :: Int, maxY :: Int
                 , rect :: Maybe (Int, Int, Int, Int), circle :: Maybe (Int, Int, Int)
                 , everything :: Bool, nether :: Bool, bottom :: Bool, sides :: Bool
                 } deriving (Show, Data, Typeable)

mc2obj :: Args
mc2obj = Args { objFolder    = "export" &= name "o"    &= help "The location of the .obj file" &= typFile
              , worldFolder  = ""       &= argPos 0 &= typ "WORLD_DIRECTORY"
              , minY         =       0                 &= help "minimum Y coordinate (bedrock = 0)"
              , maxY         =     127                 &= help "maximum Y coordinate (sealevel = 63)"
              , rect         = Nothing  &= name "r"    &= help "rectangle minX,minZ,maxX,maxZ"
              , circle       = Nothing  &= name "c"    &= help "circle centerX,centerZ,radius"
              , everything   =   False  &= name "e"    &= help "export entire world"
              , nether       =   False  &= name "n"    &= help "output Nether instead of regular world"
              , bottom       =   False  &= name "b"    &= help "output bottom of world"
              , sides        =   False  &= name "s"    &= help "output sides of world"
              } &= summary version

version :: String
version = "mc2obj v0.3\nA program to convert Minecraft worlds to .obj files\nWritten by Remco Niemeijer (2011)"

main :: IO ()
main = do arguments <- E.onException (cmdArgs mc2obj) (hFlush stderr >> putStrLn "\nUse mc2obj -? for help information")
          let regionFolder = worldFolder arguments </> if nether arguments then "DIM-1/region" else "region"
          regions <- getDirectoryContents regionFolder
          let allChunks = concat [ [ (x,z) | x <- [rx*32..rx*32+31], z <- [rz*32..rz*32+31]]
                                 | region <- regions, let (filename, ext) = splitExtension region
                                 , ext == ".mcr", let (_:rx:rz:_) = map read $ splitOn "." filename]
          let chunks = case (rect arguments, circle arguments, everything arguments) of
                            (Just (x1,z1,x2,z2),_,_) -> [(x,z) | x <- [x1..x2], z <- [z1..z2]]
                            (_,Just (cx,cz,r),_)     -> [(x,z) | x <- [cx-r..cx+r], z <- [cz-r..cz+r]
                                                               ,fromIntegral ((x-cx)*(x-cx)) + fromIntegral ((z-cz)*(z-cz)) <
                                                                (fromIntegral r+1/2)*(fromIntegral r+1/2)]
                            (_,_,True)               -> allChunks
                            _ -> []
          let worldName = takeFileName $ worldFolder arguments
          let texFolder = "texsplit/tex"
          createDirectoryIfMissing True $ objFolder arguments
          createDirectoryIfMissing True $ objFolder arguments </> "tex"
          copyFile "material/minecraft.mtl" (objFolder arguments </> "minecraft.mtl")
          mapM_ (\x -> copyFile x $ objFolder arguments </> "tex" </> takeFileName x) =<<
              filterM doesFileExist . map (texFolder </>) =<< getDirectoryContents "texsplit/tex"
          let suffix = case (rect arguments, circle arguments, everything arguments) of
                            (Just (x1,z1,x2,z2),_,_) -> printf "_r%d.%d.%d.%d" x1 z1 x2 z2
                            (_,Just (cx,cz,r),_)     -> printf "_c%d.%d.%d" cx cz r
                            (_,_,True)               -> "_e"
                            _                        -> ""
          case chunks of
               [] -> putStrLn "Error: no region specified or incorrect region"
               cs -> export ExportOptions { showBottom = bottom arguments
                                          , showSides  = sides arguments
                                          , yFrom = minY arguments, yTo = maxY arguments }
                            blockDefs regionFolder
                            (objFolder arguments </> worldName ++ suffix <.> "obj") cs