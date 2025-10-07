import qualified Data.List as List
import Text.Printf (printf)

makeLists [] = ([], [])
makeLists (h1:h2:rest)  =
    let (t1, t2) = makeLists rest in
    (h1:t1, h2:t2)
        

main = do
    line <- readFile "./input.txt"
    let w = words line 
    let (l1, l2) = makeLists w
    let 
        l1_int = map (read :: String -> Int) l1
        l2_int = map (read :: String -> Int) l2
        ol1 = List.sort l1_int
        ol2 = List.sort l2_int
        sum = foldr (\(e1, e2) acc -> acc + abs (e1 - e2)) 0 (zip ol1 ol2)
    printf "Sum: %d\n" sum

