import Control.Monad

type KnightPos = (Int, Int)

availablePos :: KnightPos -> [KnightPos]
availablePos (x, y) = do
    (x', y') <- [(x + 2, y - 1), (x + 2, y + 1), (x - 2, y - 1), (x - 2, y + 1),
                 (x + 1, y - 2), (x + 1, y + 2), (x - 1, y - 2), (x - 1, y + 2)]
    guard (x' `elem` [1..8] && y' `elem` [1..8])
    return (x', y')

withLog :: [KnightPos] -> [[KnightPos]]
withLog xs = availablePos (last xs) >>= \x -> [xs ++ [x]]

inNLog :: (Num a, Eq a) => a -> KnightPos -> [[KnightPos]]
inNLog 0 start = return [start]
inNLog n start = inNLog (n - 1) start >>= withLog

inThreeLog = inNLog 3

canReachLog :: KnightPos -> KnightPos -> [[KnightPos]]
canReachLog start end = do
    pos <- inThreeLog start
    guard (last pos == end)
    return pos

main = putStrLn $ show $ canReachLog (6, 2) (6, 1)
