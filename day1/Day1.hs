import qualified Data.List as List
import Text.Printf (printf)

readLists = do
    line <- getLine
    case line of
        "" -> return ([], [])
        l -> 
            let 
                w = map (read :: String -> Int) (words l)
                nextLine = readLists
            in
            nextLine >>= \l ->
                let (l1, l2) = l in
                return (w!!0 : l1, w!!1 : l2)


main = do
    lists <- readLists
    let 
        (l1, l2) = lists
        ol1 = List.sort l1
        ol2 = List.sort l2
        sum = foldr (\(e1, e2) acc -> acc + abs (e1 - e2)) 0 (zip ol1 ol2)
    printf "Sum: %d\n" sum

