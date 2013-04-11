import qualified Data.Map as Map


dispatch :: (Floating a, Read a) => Maybe [a] -> String -> Maybe [a]
dispatch (Just (x:y:stack)) "+" = Just $ (x + y):stack
dispatch (Just (x:y:stack)) "-" = Just $ (y - x):stack
dispatch (Just (x:y:stack)) "*" = Just $ (x * y):stack
dispatch (Just (x:y:stack)) "/" = Just $ (y / x):stack
dispatch (Just (x:y:stack)) "**" = Just $ (x ** y):stack
dispatch stack "^" = dispatch stack "**"
dispatch (Just stack) token
    | null num = Nothing
    | otherwise = Just $ (fst (head num)):stack
    where num = reads token

rpn :: (Floating a, Read a) => String -> Maybe a
rpn line = case (foldl dispatch (Just []) $ words line) of
    Nothing -> Nothing
    Just [] -> Nothing
    Just (x:_) -> Just x

main = do
    putStrLn "> "
    line <- getLine
    if null line then
        return ()
    else do
        putStrLn . show $ rpn line
        main
