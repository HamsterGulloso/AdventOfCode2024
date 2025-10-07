checkOk list =
    let
        checkOkDirection (x:y:tail) direction =
            let next_d = compare y x in
            let diff = abs (x - y) in
            (diff >= 1 && diff <= 3) 
            && case (direction, next_d) of
                (d, EQ) -> checkOkDirection (y:tail) d
                (EQ, d) -> checkOkDirection (y:tail) d
                (a, b) -> (a == b) && checkOkDirection (y:tail) direction
        checkOkDirection _ _ = True
    in
    checkOkDirection list EQ

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
