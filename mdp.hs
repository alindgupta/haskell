{-- Implementation of simple MDP in Haskell --}
{-- Alind Gupta 2016/12 --}

-- fish val out of function/container
infixr 5 ~> 
(~>) :: a -> b -> b
(~>) f a = a 

gamma = 0.9

type Probability = Double
type Reward = Double
type State = (Int,Int)

data Action = Stay | GoLeft | GoRight | GoDown | GoUp
    deriving (Show, Read, Eq, Ord, Enum)

actionLookup = 
    [ (Stay,    0)
    , (GoLeft,  1)
    , (GoRight, 2)
    , (GoDown,  3)
    , (GoUp,    4) ]


-- define static grid, invalid states marked with (1/0) == Inf
grid :: [[Reward]]
grid = [[-0.04, -0.04, -0.04,  1.0],
        [-0.04, (1/0), -0.04, -1.0],
        [-0.04, -0.04, -0.04, -0.04]]


-- valid index, includes sinks and invalid states
validIndex :: (Int,Int) -> Bool
validIndex (x,y) = if 0 <= x && x < nrows &&
                      0 <= y && y < ncols
                   then True
                   else False
                     where nrows = length grid
                           ncols = length $ head grid


-- valid state, excludes invalid states (marked with Inf on grid)
validState :: State -> Bool
validState (x,y)
    | validIndex (x,y) && not (grid !! x !! y == (1/0)) = True
    | otherwise = False


-- reward from state, returns Nothing if state is invalid
reward :: State -> Maybe Reward
reward (x,y) = if validState (x,y)
               then Just $ grid !! x !! y
               else Nothing


-- returns all moves from state, legal and illegal
-- Enum order:         GoLeft   GoRight  GoDown   GoUp
totalMoveSet :: State -> [(Int,Int)]
totalMoveSet (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]


-- returns all valid moves from state
validMoveSet :: State -> Maybe [(State)]
validMoveSet (x,y)  
    | validState (x,y) && not (sink (x,y))= Just $ filter validState $ totalMoveSet (x,y)
    | otherwise = Nothing


-- returns True if state (arg1) reachable from state (arg2)
reachable :: State 	-- end state
          -> State      -- start state
          -> Bool
reachable (x2,y2) (x1,y1)  
    | (elem (x2,y2)) (filter validState ((totalMoveSet (x1,x2)))) && not (sink (x1,y1)) = True
    | otherwise = False


-- returns True if state is a sink
sink :: State -> Bool
sink (x,y) = if reward' (x,y)  == Just 1 
             then True 
             else False
           where reward' = fmap abs . reward


-- transition probabilities of an action in state 
transitionProbs :: State -> Action -> [(State, Probability)] 
transitionProbs (x,y) a = do
        s <- zip (totalMoveSet (x,y)) (probsMatrix !! a')
        if sink (x,y)
            then return ((x,y), 0.25)
            else if validState (fst s)
                then return s
                else return (remit s) 
      where
        a' = ( (\(Just t) -> t) $ lookup a actionLookup) - 1
        remit ((a,b),c) = ((x,y),c)
        probsMatrix = [[0.8, 0.01, 0.04, 0.04],
                       [0.01, 0.8, 0.04, 0.04],
                       [0.04, 0.04, 0.8, 0.01],
                       [0.04, 0.04, 0.01, 0.8]]
   
{--
	For each state in grid, 
	for each state in

type Value = Double
-- only one state
valueIteration' :: State -> [Value] -> Double -> [Value]
valueIteration' s v d
    | d < 0.001 = v
    | otherwise = do
        

let allProbs s = fmap (transitionProbs s) [(GoLeft)..GoUp]

--}         



























 





























      
