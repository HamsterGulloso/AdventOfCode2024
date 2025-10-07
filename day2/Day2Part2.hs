import Debug.Trace
import Text.Printf (printf)

checkOk list =
    let
        checkOkDampener (x:y:z:tail) direction mistakes_left =
            let dir1 = compare y x in
            let dir2 = compare z y in

            let diff1 = abs (x - y) in
            let diff2 = abs (z - y) in

            let okDiff d = d >= 1 && d <= 3 in

            let 
                okDir EQ _ = True
                okDir _ EQ = True
                okDir d1 d2 = d1 == d2 
            in

            let okDir3 d1 d2 d3 = okDir d1 d2 && okDir d2 d3 in

            let next_direction = if EQ == dir1 then if dir2 == EQ then direction else dir2 else dir1 in

            let can_proceed = okDiff diff1 && okDiff diff2 && okDir3 direction dir1 dir2 in

            if can_proceed then
                checkOkDampener (y:z:tail) next_direction mistakes_left
            else
                mistakes_left > 0 && checkOkDampener (x:z:tail) next_direction (mistakes_left-1)
        checkOkDampener (x:y:tail) direction mistakes_left =
            let next_d = compare y x in
            let diff = abs (x - y) in
            let 
                okDir EQ _ = True
                okDir _ EQ = True
                okDir d1 d2 = d1 == d2 
            in
            (mistakes_left > 0) || (diff >= 1 && diff <= 3 && okDir direction next_d)            
        checkOkDampener _ _ _ = True
    in
    checkOkDampener list EQ 1

split :: Char -> String -> [String]
split _ "" = []
split char (c:rest) =
    let result = split char rest in
    case (c == char, result) of
        (True, head:tail) -> "":head:tail
        (True, []) -> [""]
        (False, head:tail) -> (c:head):tail
        (False, []) -> [c:""]
    

main = do
    file_content <- readFile "./input.txt"
    let file_lines = split '\n' file_content
    let str_lists = map (split ' ') file_lines
    let lists = map (map (read :: String -> Int)) str_lists
    -- print lists
    let checked = map checkOk lists
    -- print checked
    let count = foldr (\b acc -> acc + (if b then 1 else 0)) 0 checked
    print count
