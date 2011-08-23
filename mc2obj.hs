import ObjExport

main :: IO ()
main = exportWith (defaultOptions { yFrom = 60, yTo = 127 })
                  "e:/mc2obj/regions" "e:/mc2obj/test.obj"
                  [(x,z) | x <- [0..31], z <- [0..31]]