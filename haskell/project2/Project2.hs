--Project2
--Student Num 929395
--April 7th 2018

module Project2 (initialGuess, nextGuess, GameState) where
import Data.List
import System.Random

--GameState consists of a list of all possible remaining targets
type GameState = [[[Char]]]

--generates the list of possible targets for a size of x
--takes an initial guess from halfway through this list
--returns the initial guess and the initial GameState
initialGuess :: Int -> ([String], GameState)
initialGuess x = (targets !! ((length(targets))`div`2), targets)
             where targets = possibleTargets x
                   

--filters the list of targets based on the reponse
--chooses a new guess from the middle of this new list
--returns this guess and the new list of possible targets as the GameState
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (guess, targets) answer = 
      let newTargets = filterTargets guess answer targets
          newGuess = newTargets !! ((length(newTargets))`div`2)
      in (newGuess, newTargets)


--Takes the guess, the answer to that guess and the list of curent possible targets
--it then filters all of the possible targets whose response based on that guess does not match the actual response received
--result is the new list of possible targets
filterTargets :: [String] -> (Int, Int, Int) -> [[[Char]]] -> [[[Char]]]
filterTargets guess answer prevTargets = filter (\x -> (response x guess) == answer) prevTargets


--takes a size
--flattens the result of combsWithRep to generate a list of all possible combinations of pieces
possibleTargets :: Int -> [[[Char]]]
possibleTargets x = concat $ combsWithRep x pieces
         where pieces = ["WP", "WP", "WP", "WP", "WP", "WP", "WP", "WP", "BP", "BP", "BP", "BP", "BP","BP", "BP","BP", "WN", "WN", "BN", "BN", "WR", "WR", "BR", "BR", "WB", "WB", "BB", "BB", "WQ", "BQ", "WK", "BK"]                                                                                                                 


--Takes a size and a list of strings
--Generates all possible combinations of the strings up to size, including repetition
--returns a list of list of strings - one list per length of combination 
--ie list for length 0,1,2,...,k-1,k
combsWithRep :: Int -> [[Char]] -> [[[[Char]]]]
combsWithRep k xs = take (k+1) (combsBySize xs)
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x = scanl1 $ (++) . map (x :)


--Both response and mintersect are taken directly from the Project2Test code
--They are used to generate the expected response when filtering the targets
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightKind, rightColor)
  where 
        common      = mintersect guess target
        right       = length common
        rguess      = foldr (delete) guess common
        rtarget     = foldr (delete) target common
        rightColor  = length $ mintersect (map (!!0) rguess) (map (!!0) rtarget)
        rightKind   = length $ mintersect (map (!!1) rguess) (map (!!1) rtarget)

mintersect :: Eq t => [t] -> [t] -> [t]
mintersect [] _ = []
mintersect (x:xs) r = if elem x r then x : mintersect xs (delete x r)
                      else mintersect xs r 


