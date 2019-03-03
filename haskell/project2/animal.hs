> import Data.Char

> main :: IO ()
> main = do
>       mainloop $ Leaf "penguin"
>       bye

> mainloop :: DTree -> IO ()
> mainloop dtree = do
>    putStrLn ""
>    putStrLn "Think of an animal."
>    putStr "Hit return when you are ready. "
>    getLine
>    dtree' <- animalRound dtree 0
>    again <- yesOrNo "Play again? "
>    if again then mainloop dtree' else return ()

> bye :: IO ()
> bye = putStrLn "Thanks for playing!"

> yesOrNo :: String -> IO Bool
> yesOrNo prompt = do
>    response <- promptedRead  $ prompt ++ " (y/n) "
>    case dropWhile isSpace response of
>       []        -> yesOrNoAgain prompt
>       (char:_) -> case toLower char of
>                      'y' -> return True
>                      'n' -> return False
>                      (_) -> yesOrNoAgain prompt

> yesOrNoAgain :: String -> IO Bool
> yesOrNoAgain prompt = do
>       putStrLn "Please answer yes or no."
>       yesOrNo prompt

> promptedRead :: String -> IO String
> promptedRead prompt = do
>       putStr prompt
>       response <- getLine
>       let response' = dropWhile isSpace response
>       if response == [] then do
>           putStrLn "Please answer the question."
>           promptedRead prompt
>         else return response'

> data DTree = Choice String DTree DTree
>                | Leaf String

> animalRound :: DTree -> Int -> IO DTree
> animalRound (Leaf animal) depth = do
>       answer <- yesOrNo $ "Is it a " ++ animal ++ "?"
>       if answer then do
>           putStrLn $ "I guessed it with " ++ show depth ++ " questions."
>           return $ Leaf animal
>         else do
>           animal' <- promptedRead "OK, I give up.  What is your animal? "
>           putStrLn $ "Please type a question that would distinguish a "
>                   ++ animal' ++ " from a " ++ animal ++ "."
>           question' <- promptedRead "Question: "
>           answer' <- yesOrNo $ 
>                  "What is the answer to this question for a "
>                  ++ animal' ++ "?"
>           return $ Choice question'
>                 (Leaf $ if answer' then animal' else animal)
>                 (Leaf $ if answer' then animal else animal')
> animalRound (Choice question yesTree noTree) depth = do
>       answer <- yesOrNo question
>       dtree <- animalRound (if answer then yesTree else noTree) (depth+1)
>       return $ Choice question 
>               (if answer then dtree else yesTree) 
>               (if answer then noTree else dtree)
