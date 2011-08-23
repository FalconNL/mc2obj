import ObjExport

main :: IO ()
main = exportWith (defaultOptions { yFrom = 60, yTo = 127 })
                  "e:/mc2obj/regions" "e:/mc2obj/test.obj"
                  [(x,z) | x <- [-16..15], z <- [-16..15]]