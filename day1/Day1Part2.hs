makeLists [] = ([], [])
makeLists (h1:h2:rest)  =
    let (t1, t2) = makeLists rest in
    (h1:t1, h2:t2)
        
similarityScore :: (Eq n, Num n) => [n] -> n -> n
similarityScore [] _ = 0
similarityScore (head:tail) element =
    similarityScore tail element + if element == head then element else 0

main = do
    line <- readFile "./input.txt"
    let w = words line 
    let (l1, l2) = makeLists w
    let l1_int = map (read :: String -> Int) l1
    let l2_int = map (read :: String -> Int) l2
    let simillarity_sum = sum $ map (similarityScore l2_int) l1_int
    print simillarity_sum
