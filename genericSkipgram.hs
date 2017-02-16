{-# OPTIONS_GHC -fwarn-unused-matches -fwarn-incomplete-patterns  #-}

-- generic version, right context
skipgram :: [Int] -> Int -> Int -> [[Int]]
skipgram [] _ _ = [[]]
skipgram (x:xs) ng ns = skipgram' (x:xs) ng ns ns 0
  where skipgram' :: [Int] -> Int -> Int -> Int -> Int -> [[Int]]
        skipgram' [] _ _ _ _ = [[]]
        skipgram' (x:_) 1 _ _ _ = [[x]]
        skipgram' (x:xs) g s c i =
          concat [(x:) <$> (skipgram' (drop c' xs) (g-1) s c' (i+1)) 
                   | c' <- range s c i]
        range s c i = if i == 0
                        then [0..s]
                        else [0..(s-c)]

-- rolling along the given list
roll :: [Int] -> Int -> Int -> [[Int]]
roll [] _ _ = []
roll (x:xs) n s = skipGram (x:xs) n s ++ roll xs n s


-- wrapper for skipgram to pad -1
skipGram :: [Int] -> Int -> Int -> [[Int]]
skipGram [] _ _ = [[]]
skipGram x n s = skipgram (x ++ repeat (-1)) n s



