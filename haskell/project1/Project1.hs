--Declarative Programming Project 1
--Monday 19th March 2018
--Eve Byrne

module Project1 (elementPosition, everyNth, elementBefore) where 

--returns first position of element in a list, or 0 if element not in list
elementPosition :: Eq t => t -> [t] -> Int
elementPosition a (x:xs)
          | (elem a (x:xs)) == False = 0
          | a == x = 1
          | otherwise = 1 + elementPosition a xs


--returns a new list with everyNth element of the original
everyNth :: Int -> [t] -> [t]
everyNth n = go n where
            go 0 xs = error "n can't be 0"
            go _ [] = []
            go 1 (x:xs) = x:(go n xs)
            go k (x:xs) = go (k-1) xs

--returns element before e 
elementBefore :: Eq a => a -> [a] -> Maybe a
elementBefore _ (x:[]) = Nothing
elementBefore e (x:y:xs)
               | e == x = Nothing
               | e == y = Just x
               | otherwise = elementBefore e (y:xs)

