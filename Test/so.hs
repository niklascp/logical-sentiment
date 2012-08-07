{-# LANGUAGE ImplicitParams #-}

f :: (?x :: Int) => Int -> Int
f n = n + ?x

g :: (Int -> Int) -> (Int -> Int)
g t = let ?x = 5 in t
