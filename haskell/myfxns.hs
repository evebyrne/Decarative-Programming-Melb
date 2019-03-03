import Data.List 
import qualified Data.Map as Map 
import qualified Data.Set as Set  
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S 



--BST = all elements on left subtree < element at that root
--                      right        >
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord) 

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 

--creates a leaf node
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  


treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
        | x == a = Node x left right  
        | x < a  = Node a (treeInsert x left) right  
        | x > a  = Node a left (treeInsert x right)  

list_to_BST :: (Ord a) => [a] -> Tree a
list_to_BST [] = EmptyTree
list_to_BST (x:xs) = treeInsert x (list_to_BST xs)

inOrder :: Tree a -> [a]
inOrder EmptyTree = []
inOrder (Node x left right) = (inOrder left) ++ [x] ++ (inOrder right) 

inOrderListToTree :: (Ord a) => [a] -> [a]
inOrderListToTree [] = []
inOrderListToTree xs = inOrder (list_to_BST xs)


instance Functor Tree where  
        fmap f EmptyTree = EmptyTree  
        fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 

data Tree2 k v = Leaf | Node2 k v (Tree2 k v) (Tree2 k v) deriving (Eq, Show)

--same structure?
same_shape :: Tree2 a b -> Tree2 c v -> Bool
same_shape Leaf Leaf = True
same_shape (Node2 _ _ _ _) Leaf = False
same_shape Leaf (Node2 _ _ _ _) = False
same_shape (Node2 _ _ l1 r1) (Node2 _ _ l2 r2) 
        = same_shape l1 l2 && same_shape r1 r2
 

--    data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  
data LockerState = Taken | Free deriving (Show, Eq)  

type Code = String  

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> String  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing ->  "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then code  
                                else  "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
	[(100,(Taken,"ZD39I"))  
	,(101,(Free,"JAH3I"))  
	,(103,(Free,"IQSA9"))  
	,(105,(Free,"QOTSA"))  
	,(109,(Taken,"893JJ"))  
	,(110,(Taken,"99292"))  
	]  
   


first :: (a, b, c) -> a  

first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z 

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs  

sumF :: (Num a) => [a] -> a  
sumF = foldl (+) 0 

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b 

--"|" as "such that" and " <-" as "drawn from". 
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
     let smallerSorted = quicksort [a | a <- xs, a <= x]  
         biggerSorted = quicksort [a | a <- xs, a > x]  
     in  smallerSorted ++ [x] ++ biggerSorted  


quicksortf :: (Ord a) => [a] -> [a]    
quicksortf [] = []    
quicksortf (x:xs) =     
   let smallerSorted = quicksortf (filter (<=x) xs)  
       biggerSorted = quicksortf (filter (>x) xs)   
   in  smallerSorted ++ [x] ++ biggerSorted  

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) =
     let smaller = [a | a <- xs, a <= x]
         bigger = [a | a <- xs, a > x]
     in (qs smaller) ++ [x] ++ (qs bigger)

map1 :: (a -> b) -> [a] -> [b]  
map1 _ [] = []  
map1 f (x:xs) = f x : map f xs 




map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  


elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys 





multThree :: (Num a ) => a -> a -> a -> a
multThree x y z = x * y * z



compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f $ f x


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

flip' :: (a -> b -> c) -> b -> a -> c 
flip' f x y = f y x



filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs  

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter (\x -> x `mod` 3829 == 0) [100000,99999..])  
    --where p x = x `mod` 3829 == 0  


chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)

numLongChains:: Int
numLongChains = length(filter isLong (map chain [1..100]))
     where isLong xs = length xs > 15

numLongChainsL = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  


phoneBook0 =   
	[("betty","555-2938")  
	,("bonnie","452-2928")  
	,("patsy","493-2928")  
	,("lucille","205-2928")  
	,("wendy","939-8282")  
	,("penny","853-2492")
	,("wendy","978-0000")  
	]
phoneBook =   
        [("betty","555-2938")  
        ,("betty","342-2492")  
        ,("bonnie","452-2928")  
        ,("patsy","943-2929")  
        ,("patsy","827-9162")  
        ,("lucille","205-2928")  
        ,("wendy","939-8282")  
        ,("penny","853-2492")  
	,("patsy","493-2928")  
        ,("penny","555-2111")  
        ]  

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs  

findKey :: (Eq k) => k -> [(k,v)] -> v  
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs   

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey' key [] = Nothing  
findKey' key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey' key xs  

findKeyF :: (Eq k) => k -> [(k,v)] -> Maybe v 
findKeyF key = foldr (\(k,v) acc-> if key == k then  Just v else acc) Nothing 



data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int   
                     } deriving (Show, Read, Eq) 


data Vector a = Vector a a a deriving (Show)  

vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  

vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  

scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  

factorial101 :: Int -> Int 
factorial101 1 = 1
factorial101 n = n * factorial101 (n-1)
  
append::[a] ->  [a] -> [a]
append [] xs = xs
append xs [] = xs
append (x:xs) ys = x:append xs ys

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = (reverse1 xs) ++ [x]

getN :: Int -> [a] -> a
getN n [] = error "n is too big"
getN n xs  
       | n<1 = error "n is too small" 
       | n==1 = head xs
       | otherwise = getN (n-1) (tail xs)

data Font_colour = Colour_name String | Hex Int | RGB Int Int Int deriving (Show)
data Font_tag = Font_tag (Maybe Int) (Maybe String) (Maybe Font_colour) deriving(Show)


myElem ::(Eq a) => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) 
       | a == x = True
       | otherwise = myElem a xs


longestPrefix ::(Eq a) =>  [a] -> [a] -> [a]
longestPrefix [] _ = []
longestPrefix _ [] = []
longestPrefix (x:xs) (y:ys) 
           | x==y = x:(longestPrefix xs ys)
           | otherwise = []

mywhile x = 
        if cond x then mywhile (next_version_of x)
        else final_version_of x

--cond (c,n) = c /= 0
--next_version_of (c,n) = 
  --      if n>100 then (c-1,n-10) else (c+1,n+11)
--final_version_of (c,n) = n
  

cond xs = xs /= 23
next_version_of x = x + 1
final_version_of x = x + 10          

--mycarthy_91 :: Int -> Int
--mycarthy_91 n = mywhile(1,n)
  

my = mywhile 0        
minmax :: Int -> Int -> [Int]
minmax x y 
    | x==y = []
    | x > y = error "min gt max"
    | otherwise = x:(minmax (x+1) y)

--ftoc :: Double -> Double
ftoc f = (5/9) * (f-32)

quadroots :: Double -> Double -> Double -> [Double]
quadroots 0 0 _ = error "at least a or b need to be zero"
quadroots 0 b c = [-c/b]
quadroots a b c 
       | disc < 0 = error "no real solution"
       | disc == 0 = [tmp]
       | otherwise = [tmp + t, tmp-t]
       where disc = b*b - 4*a*c
             tmp = -b /(2*a)
             t = sqrt(disc) / (2*a)
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
      | x < y = x:(merge xs (y:ys))
      | x==y = x:y:(merge xs ys)
      | x>y = y:(merge (x:xs) ys)




{-data Expression
  --     = Var Variable
    --   | Num Integer
      -- | Plus Expression Expression
       --| Minus Expression Expression
      -- | Times Expression Expression
       --| Div Expression Expression
-}
data Expr 
      = Number Int | Variable String | Binop Binop Expr Expr | Unop Unop Expr 

data Binop = Plus | Minus | Times | Divide
data Unop = Negate

--data Variable = A | B

--expr = Plus (Times (Num 2) (Var A)) (Var B)

--eval :: Integer -> Integer -> Expression -> Integer
{-eval a b (Var A) = a
eval a b (Var B) = b
eval a b (Num n) = n
eval a b (Plus ex1 ex2) = (eval a b ex1) + (eval a b ex2)
eval a b (Minus ex1 ex2) = (eval a b ex1) - (eval a b ex2)
eval a b (Times ex1 ex2) = (eval a b ex1) * (eval a b ex2)
eval a b (Div ex1 ex2) = (eval a b ex1) `div` (eval a b ex2)
-}

is_static :: Expr -> Bool
is_static expr = 
     case expr of 
            Number _ -> True
            Variable _ -> False
            Unop _ expr1 -> is_static expr1
            Binop _ expr1 expr2 -> is_static expr1 && is_static expr2
          
is_atomic :: Expr -> Bool
is_atomic expr = 
       case expr of 
           Unop _ _ -> False
           Binop _ _ _ -> False
           _ -> True

elementPosition :: Eq t => t -> [t] -> Int
elementPosition a (x:xs) 
          | (elem a (x:xs)) == False = 0
          | a == x = 1
          | otherwise = 1 + elementPosition a xs



everyNth :: Int -> [t] -> [t]
everyNth n = go n where
            go 0 xs = error "n can't be 0"
            go _ [] = []
            go 1 (x:xs) = x:(go n xs)
            go k (x:xs) = go (k-1) xs

elementBefore :: Eq a => a -> [a] -> Maybe a
elementBefore _ (x:[]) = Nothing
elementBefore e (x:y:xs)
               | e == x = Nothing
               | e == y = Just x
               | otherwise = elementBefore e (y:xs)

myFold :: (v -> a -> v)-> v-> [a] -> v
myFold _ base [] = base
myFold f base (x:xs) = myFold f (f base x) xs


myTranspose :: [[a]] -> [[a]]
myTranspose [] = error "cant find transpose of an empty list"
myTranspose list@(xs:xss) 
         | len > 0 = transpose' len list
         | otherwise = error  "empty inner list?"
         where len = length xs

transpose' len [] = replicate len []
transpose' len (xs:xss) 
       | len == length xs = zipWith (:) xs (transpose' len xss)
       | otherwise = error "inner list of different lengths"


--list_stats :: [a] -> (a,a,a)
list_stats [] = (0,0,0)
list_stats xs = ((length xs), (foldl (+) 0 xs), (foldr (\x -> ((+)(x^2))) 0 xs)) 

stats1 xs = (length xs, sum xs, sumsq xs)

sumsq [] = 0
sumsq (x:xs) = x*x + sumsq xs

stats2 [] = (0,0,0)
stats2 (x:xs) = 
     let (l,s,sq) = stats2 xs
     in (l+1, s+x, sq+x*x)

--tute 5
maybeApply :: (a->b) -> Maybe a -> Maybe b 
maybeApply _ Nothing = Nothing
maybeApply f (Just x) = Just (f x)

zipW :: (a->b->c) -> [a] -> [b] -> [c]
zipW _ _ [] = []
zipW _ [] _ = []
zipW f (x:xs) (y:ys) = (f x y):(zipW f xs ys)

linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn _ _ [] = []
linearEqn y z (x:xs) = (y*x + z):(linearEqn y z xs)

linearEqn2 :: Num a => a -> a -> [a] -> [a]
linearEqn2 m n = map (\x -> m*x + n)


sqrtPN :: (Floating a, Ord a) => a -> [a]
sqrtPN x
   | x > 0 = let y = sqrt x in [y, -y]
   | x == 0 = [0]
   | otherwise = []

allSqrts :: (Floating a, Ord a) => [a] -> [a]
allSqrts [] = []
allSqrts (x:xs) = (sqrtPN x)++(allSqrts xs) 

allSqrts2 :: (Floating a, Ord a) => [a] -> [a]
allSqrts2 xs = foldl (++) [] (map sqrtPN xs)


--a
sqrtPositive :: (Floating a, Ord a) => [a] -> [a]
sqrtPositive [] = []
sqrtPositive xs = map (sqrt) (filter (>=0) xs)

--b
sqrtPos2 :: (Floating a, Ord a) => [a] -> [a]
sqrtPos2 [] = []
sqrtPos2 (x:xs) 
      | x >= 0 = (sqrt x):(sqrtPos2 xs)
      | otherwise = sqrtPos2 xs
    
--transform b to a??? 

mintersect :: Eq t => [t] -> [t] -> [t]
mintersect [] _ =  []
mintersect (x:xs) r = if elem x r then x:(mintersect xs (delete x r)) 
                      else mintersect xs r

response :: [String] -> [String] -> (Int, Int, Int)
response target guess = (right, rightKind, rightColor)
   where 
       common = mintersect guess target
       right = length common
       rguess = foldr (delete) guess common 
       rtarget = foldr (delete) target common
       rightColor = length $ mintersect (map (!!0) rguess) (map (!!0) rtarget)
       rightKind = length $ mintersect (map (!!1) rguess) (map (!!1) rtarget)         



iota n = 
     if n == 0 then [] else iota (n-1) ++ [n]

iota1 n 
     | n == 0 = []
     | otherwise = iota(n-1) ++ [n]

combs3 [] = []
combs3 (x:xs) = (map (x++) (x:xs)) ++ (combs3 xs)

combs2 _ [] = []
combs2 1 xs = xs
combs2 n (x:xs) = ((zipList (replicate (length(x:xs)) x) (combs2 (n-1) (x:xs))) ++ (combs2 n xs))

combs [] =[]
combs (x:xs) =((zipList (replicate (length(x:xs)) x) (x:xs)) ++ combs xs)  

zipList [] _ = []
zipList _ [] = []
zipList (x:xs) (y:ys) = [x++y]++(zipList xs ys)



combos [] = [[]]
combos ([]:ls) = combos ls
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)


merge2 :: (Ord a) => [a] -> [a] -> [a]
merge2 x [] = x
merge2 [] x = x
merge2 (x:xs) (y:ys) 
      | x < y = x:(merge2 xs (y:ys))
      | otherwise =  y:(merge2 (x:xs) ys)

