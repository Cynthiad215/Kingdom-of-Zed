import Data.List as List
import Data.Set as Set

-- Generate all row permutations for gameboard of size n.
generate :: (Num t, Eq t, Enum t) => t -> [[[t]]]
generate n = concatMap permutations $ choose (permutations [1..n]) n
  where
    choose _ 0 = [[]]
    choose [] _ = []
    choose (x:xs) n = (List.map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

-- Solve game board given a 4-tuple containing the number of posts each merchant is to visit
solve :: ([Int],[Int],[Int],[Int]) -> Maybe [[Int]]
solve (top, right, bottom, left) = 
  findValidSolution (top, right, bottom, left) $ generate n
  where
    n = length top

-- TEST CASES
-- solve ([1,2],[2,1],[1,2],[2,1])
-- should return [[2,1],[1,2]]
-- solve ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])
-- should return [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- solve ([3,1,2,3],[3,2,1,2],[2,3,3,1],[1,2,3,2])
-- should return [[2,4,3,1],[1,3,4,2],[3,1,2,4],[4,2,1,3]]
-- solve ([2,2,1],[1,2,2],[3,1,2],[2,1,3])
-- should return [[1,2,3],[3,1,2],[2,3,1]]
-- solve ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0])
-- should return [[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
-- solve ([0,0,3,0],[0,0,2,0],[0,0,4,0],[0,0,3,0])
-- should return [[2,4,1,3],[1,3,2,4],[3,2,4,1],[4,1,3,2]]

-- Find a valid solution given merchant visit numbers and array containing all possible solutions
findValidSolution :: (Num a, Eq a) => ([a], [a], [a], [a]) -> [[[Int]]] -> Maybe [[Int]]
findValidSolution merchants [] = Nothing
findValidSolution merchants (path:paths)
  | validateSolution merchants path = Just path
  | otherwise = findValidSolution merchants paths

-- Return true if the given board is a valid solution given solution
validateSolution :: (Num a, Eq a) => ([a],[a],[a],[a]) -> [[Int]] -> Bool
validateSolution (top, right, bottom, left) solution = 
  validateMerchants top (topPaths solution) &&
  validateMerchants right (rightPaths solution) &&
  validateMerchants bottom (bottomPaths solution) &&
  validateMerchants left (leftPaths solution)
  where
    validateMerchants [] [] = True
    validateMerchants (merchant:merchants) (path:paths) =
      validatePosts merchant path 0 && validateMerchants merchants paths

-- TEST CASES
-- validateSolution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return true
-- validateSolution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[4,1,3,2],[2,3,4,1],[4,2,1,3],[1,4,2,3]]
-- should return false
-- validateSolution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]]
-- should return false
-- validateSolution ([1,2],[2,1],[1,2],[2,1]) [[2,1],[1,2]]
-- should return true
-- validateSolution ([1,2],[2,1],[1,2],[2,1]) [[1,2],[1,2]]
-- should return false
-- validateSolution ([2,2,3,4,2,1],[1,5,3,2,2,2],[2,2,1,2,3,4],[3,3,2,2,1,5]) [[1,4,3,2,5,6],[6,2,5,4,3,1],[3,6,2,5,1,4],[5,4,1,3,6,2],[4,5,6,1,2,3],[2,1,3,6,4,5]]
-- should return false
-- validateSolution ([2,2,3,4,2,1],[1,5,3,2,2,2],[2,2,1,2,3,4],[3,3,2,2,1,5]) [[1,4,3,2,5,6],[6,2,5,4,3,1],[3,6,2,5,1,4],[5,4,1,3,6,2],[4,5,6,1,2,3],[2,1,3,6,4,5]]
-- should return false
-- validateSolution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[4,2,3,1],[1,3,4,2],[2,1,3,4],[1,4,2,3]]
-- should return false
-- validateSolution ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) [[1,4,3,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
-- should return true
-- validateSolution ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) [[1,3,4,2],[2,3,1,4],[4,1,2,3],[3,2,4,1]]
-- should return false
-- validateSolution ([0,1,2,0],[0,0,0,2],[3,0,3,0],[0,0,0,0]) [[1,2,3,4],[2,4,1,3],[4,3,2,1],[3,1,4,2]]
-- should return FALSE
-- validateSolution ([0,0,3,0],[0,0,2,0],[0,0,4,0],[0,0,3,0]) [[2,4,1,3],[1,3,2,4],[3,2,4,1],[4,1,3,2]]
-- should return true
-- validateSolution ([0,0,3,0],[0,0,2,0],[0,0,4,0],[0,0,3,0]) [[1,4,2,3],[2,3,1,4],[4,2,3,1],[3,1,4,2]]
-- should return FALSE

-- Return true if the line of posts is valid 
validatePosts :: (Eq t, Num t, Num a, Ord a) => t -> [a] -> a -> Bool
validatePosts 0 [] lastVisitedPost = True
validatePosts 0 posts 0 = not $ hasDuplicates posts
validatePosts 0 (post:posts) lastVisitedPost
  | post > lastVisitedPost = False
  | otherwise = validatePosts 0 posts lastVisitedPost
validatePosts visitsLeft [] lastVisitedPost = False
validatePosts visitsLeft (post:posts) lastVisitedPost
  | hasDuplicates (post:posts) = False
  | post > lastVisitedPost = validatePosts (visitsLeft-1) posts post
  | otherwise = validatePosts visitsLeft posts lastVisitedPost

-- TEST CASES
-- validatePosts 3 [1,2,3,4] 0
-- should return false
-- validatePosts 1 [4,1,3,2] 0
-- should return true
-- validatePosts 2 [1,4,3,2] 0
-- should return true
-- validatePosts 4 [2,4,3,1] 0
-- should return false
-- validatePosts 4 [1,2,3,4] 0
-- should return true
-- validatePosts 4 [4,4,4,4] 0
-- should return false
-- validatePosts 3 [1,2,1,4] 0
-- should return false

-- Returns true if input list has duplicate value.
hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates lst = length lst /= length set
  where set = Set.fromList lst

-- Returns array containing line of trading posts corresponding to merchants on top
topPaths :: [[Int]] -> [[Int]]
topPaths rows = transpose rows

-- TEST CASES
-- topPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[4,2,3,1],[1,3,2,4],[3,4,1,2],[2,1,4,3]]

-- Returns array containing line of trading posts corresponding to merchants to the right
rightPaths :: [[Int]] -> [[Int]]
rightPaths rows = List.map reverse rows

-- TEST CASES
-- rightPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[2,3,1,4],[1,4,3,2],[4,1,2,3],[3,2,4,1]]

-- Returns array containing line of trading posts corresponding to merchants at bottom
bottomPaths :: [[Int]] -> [[Int]]
bottomPaths rows = List.map reverse $ reverse $ transpose rows

-- TEST CASES
-- bottomPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[3,4,1,2],[2,1,4,3],[4,2,3,1],[1,3,2,4]]

-- Returns array containing line of trading posts corresponding to merchants to the left
leftPaths :: [[Int]] -> [[Int]]
leftPaths rows = reverse rows

-- TEST CASES
-- leftPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[1,4,2,3],[3,2,1,4],[2,3,4,1],[4,1,3,2]]


main = do
          putStrLn "Dimension of grid"
          input1 <- getLine
          let dim = (read input1 :: Int)
          putStrLn "enter values for the top line: " 
          top <- getLine 
          let t1 = (read top :: [Int])
          putStrLn "enter values for the right column: " 
          right <- getLine 
          let r1 = (read right :: [Int])
          putStrLn "enter values for the bottom line: " 
          bottom <- getLine 
          let b1 = (read bottom :: [Int])
          putStrLn "enter values for the left line: " 
          left <- getLine 
          let l1 = (read left :: [Int])
          if ((isValid t1 dim) && (isValid r1 dim) && (isValid b1 dim) && (isValid l1 dim) 
            && (length t1 == dim) && (length r1 == dim) && (length b1 == dim) && (length l1 == dim))
           then print . solve $ (t1, r1, b1, l1)
           else do 
                print ("try again")
                main


isValid [] _ = True;
isValid (h:t) x = if (elem h [0..x]) then isValid t x else False