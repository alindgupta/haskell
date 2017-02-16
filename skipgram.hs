-- | This is an implementation of skip-gram, fairly fast
-- and does not generate duplicate elements (unproven).
-- The algorithm uses recursion trees that branch according to genRange
-- Recursion ends when nGrams equals 1 (decremented at each recursive level by 1)
-- For the helper function skipGram', two extra variables are implemented
-- c = currSkips, specifies current nSkips (as opposed to the original top-level nSkips)
-- i = iteration, specifies recursion level, needed for the helper function genRange only
    -- Note: c seems to be irrelevant as long as i is set to 0 just in terms of output 
    -- Note: i is used because top-level branching is different from non-top-level branching
    -- I couldn't think of another simpler way to do this
    
-- | Important: this algorithm only cares about right-side context
-- It is trivial to do a left+right window-based skipgram from this
-- The first element in each sublist is the woi (input for skip-gram)
-- The rest of the list is the context (for hierarchical softmax) 
-- or 

import System.Environment (getArgs)

skipGram :: [Int] -> Int -> Int -> [[Int]]
skipGram (x:xs) nGrams nSkips = skipGram' (x:xs) nGrams nSkips nSkips 0
    where skipGram' (x:xs) 1 _ _ _ = [[x]]
                                    -- do not change to concatMap, it doesn't work
          skipGram' (x:xs) g s c i = concat [(x:) <$> (skipGram' (drop c' xs) (g-1) s c' (i+1)) | c' <- genRange s c i]   
          genRange s c i = if i == 0
                           then [0..s]
                           else [0..(s-c)]

main = do
    nGrams:nSkips:_ <- getArgs
    let x = [1..]
    let output = skipGram x (read nGrams :: Int) (read nSkips :: Int)
    print output






