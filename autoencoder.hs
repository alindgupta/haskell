{- Purely list-based manipulations, very slow --}

import Data.List (transpose, intercalate)
import Data.Random (normal, sample, RVar, MonadRandom)
import Control.Monad
import Data.List.Split (chunksOf)

data Vector = Vector
    { vector :: [Double]
    , dim    :: Int
    } deriving (Show, Ord, Eq)

data Matrix = Matrix
    { matrix :: [[Double]]
    , dims   :: (Int,Int)
    } deriving (Ord, Eq)

zeros :: Int -> Vector
zeros n = Vector (replicate n 0) n

incVec :: Int -> Int -> Vector
incVec b e = Vector [b'..e'] (e - b)
    where b' = fromIntegral b
          e' = fromIntegral e

instance Show Matrix where
    show (Matrix m _) = unlines $ 
                        [intercalate "   " $ show <$> m' | m' <- m]

{-
instance Functor Matrix where
    fmap f (Matrix m d) = Matrix ((map . map) f m) d 
-}

reshapeMat :: [Double] -> Int -> Int -> [[Double]]
reshapeMat x r c = chunksOf r x


-- inner product operator
infixr 7 <@>
(<@>) :: Vector -> Vector -> Double
x <@> y = if dim x == dim y
            then sum $ zipWith (*) (vector x) (vector y)
            else error "Lengths do not match"

-- generic inner product (deprecate when matrix :: [Vector])
infixr 7 <@@>
(<@@>) :: [Double] -> [Double] -> Double
x <@@> y = sum $ zipWith (*) x y

-- matrix multiplication
matT :: Matrix -> Matrix
matT (Matrix m (r,c)) = Matrix (transpose m) (c,r)

matT_generic :: Num a =>[[a]] -> [[a]]
matT_generic x = if all (== (length . head) x) (map length x)
                    then transpose x
                    else error "Inconsistent dimensions!"

crossEntropyLoss :: Vector -> Vector -> Double
crossEntropyLoss p q    -- q is approximation to p
    | dim p == 0 || dim q == 0 = error "Empty vector(s)"
    | dim p /= dim q   = error "Unequal lengths"
    | otherwise = (negate . sum) $ 
                  (\(a,b) -> a*b) <$> 
                  zip (vector p) (fmap log (vector q))  

matMul :: Matrix -> Matrix -> Matrix
matMul w x = Matrix
    ([[((matrix w) !! r) <@@> j | j <- (matrix x')] |
        r <- [0..((fst $ dims w) - 1)]])
    (fst $ dims w, snd $ dims x)
  where x' = matT x

matAdd :: Matrix -> Matrix -> Matrix
matAdd w x = Matrix (zipWith (zipWith (+)) (matrix w) (matrix x)) (dims w)

matMin :: Matrix -> Matrix -> Matrix
matMin w x = Matrix (zipWith (zipWith (-)) (matrix w) (matrix  x)) (dims w)

infixr 7 <+>
(<+>) :: Matrix -> Matrix -> Matrix
a <+> b = if dims a == dims b 
              then matAdd a b
              else error "Dimension mismatch"

infixr 7 <->
(<->) :: Matrix -> Matrix -> Matrix
a <-> b = if dims a == dims b
              then matMin a b
              else error "Dimension mismatch"

initMatrix :: MonadRandom m0 => Int -> Int -> Double -> Double -> m0 Matrix
initMatrix r c m sd = do
    x <- replicateM (r * c) $ sample $ (normal m sd :: RVar Double)
    return $ Matrix (reshapeMat x r c) (r,c)

initMatrix' :: MonadRandom m0 => Int -> Int -> Double -> Double -> m0 Matrix
initMatrix' r c m sd = 
    (replicateM (r * c) $ sample $ (normal m sd :: RVar Double)) >>=
        (\x -> return $ Matrix (reshapeMat x r c) (r,c))

{-
main = do
           let x = "some input matrix"
           wi <- initMatrix 2 2 1 1
           wo <- initMatrix 2 2 1 1
           let state  = matMul input wi
           let output = matMul state wo
           let difference = input <-> output 
           let loss = crossEntropyLoss difference

           whileM_ (100 cycles)
               --run loop and propagate gradients
-}

