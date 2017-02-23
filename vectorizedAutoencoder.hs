import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M


-- | Initialize a vector of zeros
zeros :: Int -> V.Vector Double
zeros = flip V.replicate 0.0

-- | Initialize with random
initMatrix :: MonadRandom m => Int
                            -> Int
                            -> Double
                            -> Double
                            -> m V.Vector (V.Vector Double)
initMatrix r c m sd = do
    x <- V.replicateM (r*c) $ sample $ normal m sd :: RVar Double
    return $ reshapeMat x r c
      where reshapeMat :: 

-- | Initialize a vector in range
incVec :: Int -> Int -> V.Vector Double
incVec = (V.fromList .) . enumFromTo

-- | Initialize matrix of zeros
matZero :: Int -> Int -> V.Vector (V.Vector Double)
matZero rows cols = V.replicate rows (zeros cols)

-- | inner product
infixr 7 //
(//) :: (Num a, Enum a) => V.Vector a -> V.Vector a -> Double
a // b = 


-- | cross-entropy loss
crossEntropyLoss :: (Num a, Enum a) => V.Vector a -> V.Vector a -> Double
crossEntropyLoss p q = negate . V.sum $ V.zipWith (*) p (V.map log q)





