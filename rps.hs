import Control.Applicative

data Hand = Rock | Paper | Scissors deriving (Show, Eq, Read)

instance Ord Hand where
    Rock <= Paper = True
    Paper <= Scissors = True
    Scissors <= Rock = True
    a <= b
        | a == b = True
        | otherwise = not $ b <= a

getResult :: Hand -> Hand -> String
getResult hand1 hand2 = do
    let
        comp = hand1 `compare` hand2
    case comp of
        GT -> "Player one wins"
        LT -> "Player two wins"
        EQ -> "Tie"

getInput :: IO Hand
getInput = do
    putStrLn "Enter Rock Paper or Scissors:"
    getLine >>= return . read

main = getResult <$> getInput <*> getInput >>= putStrLn
