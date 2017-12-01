import Data.List

-- Generate all row permutations for gameboard of size n.
generate n = concatMap permutations $ choose (permutations [1..n]) n
  where
    choose _ 0 = [[]]
    choose [] _ = []
    choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

-- Solve game board given a 4-tuple containing the number of posts each merchant is to visit
solve :: ([Int],[Int],[Int],[Int]) -> Maybe [[Int]]
solve (top, right, bottom, left) = 
  findValidSolution (top, right, bottom, left) (generate (length top))

-- TEST CASES
-- solve ([1,2],[2,1],[1,2],[2,1])
-- should return [[2,1],[1,2]]
-- solve ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1])
-- should return [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- solve ([3,1,2,3],[3,2,1,2],[2,3,3,1],[1,2,3,2])
-- should return [[2,4,3,1],[1,3,4,2],[3,1,2,4],[4,2,1,3]]
-- solve ([2,2,1],[1,2,2],[3,1,2],[2,1,3])
-- should return [[1,2,3],[3,1,2],[2,3,1]]

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
-- should return true
-- validateSolution ([2,2,3,4,2,1],[1,5,3,2,2,2],[2,2,1,2,3,4],[3,3,2,2,1,5]) [[1,4,3,2,5,6],[6,2,5,4,3,1],[3,6,2,5,1,4],[5,4,1,3,6,2],[4,5,6,1,2,3],[2,1,3,6,4,5]]
-- should return false
-- validateSolution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[4,2,3,1],[1,3,4,2],[2,1,3,4],[1,4,2,3]]
-- should return false

-- Return true if the line of posts is valid 
validatePosts :: (Eq t, Num t, Ord a) => t -> [a] -> a -> Bool
validatePosts visitsLeft [] lastVisitedPost
  | visitsLeft == 0 = True
  | otherwise = False
validatePosts visitsLeft (post:posts) lastVisitedPost
  | visitsLeft == 0 = True
  | post == lastVisitedPost = False
  | elem lastVisitedPost posts = False
  | post > lastVisitedPost = validatePosts (visitsLeft-1) posts post
  | otherwise = validatePosts visitsLeft posts lastVisitedPost

-- TEST CASES
-- validatePosts 3 [1,2,3,4] 0
-- should return true
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

-- Returns array containing line of trading posts corresponding to merchants on top
topPaths :: [[Int]] -> [[Int]]
topPaths rows = transpose rows

-- TEST CASES
-- topPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[4,2,3,1],[1,3,2,4],[3,4,1,2],[2,1,4,3]]

-- Returns array containing line of trading posts corresponding to merchants to the right
rightPaths :: [[Int]] -> [[Int]]
rightPaths rows = map reverse rows

-- TEST CASES
-- rightPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[2,3,1,4],[1,4,3,2],[4,1,2,3],[3,2,4,1]]

-- Returns array containing line of trading posts corresponding to merchants at bottom
bottomPaths :: [[Int]] -> [[Int]]
bottomPaths rows = map reverse $ reverse $ transpose rows

-- TEST CASES
-- bottomPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[3,4,1,2],[2,1,4,3],[4,2,3,1],[1,3,2,4]]

-- Returns array containing line of trading posts corresponding to merchants to the left
leftPaths :: [[Int]] -> [[Int]]
leftPaths rows = reverse rows

-- TEST CASES
-- leftPaths [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return [[1,4,2,3],[3,2,1,4],[2,3,4,1],[4,1,3,2]]