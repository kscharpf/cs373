
data Color = Red | Green deriving (Eq, Ord, Show, Read, Bounded, Enum)

type ProbabilityVector = [Float]
type PositionProbabilityVector = ProbabilityVector
type Probability = Float

world = [Green, Red, Red, Green, Green]
initialPosition :: PositionProbabilityVector
initialPosition = [0.2, 0.2, 0.2, 0.2, 0.2]

measurements :: [Color]
measurements = [Red, Green]

motions = [1,1]

pHit = 0.6
pMiss = 0.2
pUndershoot = 0.1
pExactshoot = 0.8
pOvershoot = 0.1

rotateHelper :: [a] -> Int -> [a] -> Int -> [a]
rotateHelper _ _ qs 0 = qs
rotateHelper xs n qs countDown = rotateHelper xs (n+1) (qs ++ [xs !! (n `mod` (length xs))]) (countDown - 1)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = rotateHelper xs n [] (length(xs))

cyclicMoveHelper :: PositionProbabilityVector -> Probability -> Int -> PositionProbabilityVector
cyclicMoveHelper ps p n = map (* p) (rotate ps (-n))

cyclicMove :: PositionProbabilityVector -> Probability -> Probability -> Probability -> Int -> PositionProbabilityVector
cyclicMove ps pUnder pExact pOver n = zipWith3 (\x y z -> x + y + z) (cyclicMoveHelper ps pUnder (n-1)) (cyclicMoveHelper ps pExact n) (cyclicMoveHelper ps pOver (n+1))

hitMissVal :: Probability -> Probability -> Color -> Color -> Probability
hitMissVal a b x y 
  | x == y = a
  | otherwise = b

sense :: PositionProbabilityVector -> Color -> [Color] -> Probability -> Probability -> PositionProbabilityVector
sense ps m w hitProb missProb =  map (/ sum(p)) p
                                 where p = zipWith (*) ps (map (hitMissVal hitProb missProb m) w)

senseAndMove :: PositionProbabilityVector -> [Color] -> [Int] -> PositionProbabilityVector
senseAndMove ps [] _ = ps
senseAndMove ps _ [] = ps
senseAndMove ps (m:ms) (x:xs) = senseAndMove (cyclicMove (sense ps m world pHit pMiss) pUndershoot pExactshoot pOvershoot x) ms xs

doCar = senseAndMove initialPosition measurements motions

