import Data.List

-- Generate all row permutations for gameboard of size n.
generate n = permutations [1..n]

-- Solve game board
-- TO DO
solve (top, right, bottom, left) = top

-- Return true if the given board is a valid solution given input
  validate_merchants top (top_paths input) &&
  validate_merchants right (right_paths input) &&
  validate_merchants bottom (bottom_paths input) &&
  validate_merchants left (left_paths input)
  where
    validate_merchants merchants paths
      | merchants == [] || paths == [] = True
      | otherwise = validate_posts (head merchants) (head paths) 0 && validate_merchants (tail merchants) (tail paths)

-- TEST CASES
-- is_valid_solution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]]
-- should return true
-- is_valid_solution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[4,1,3,2],[2,3,4,1],[4,2,1,3],[1,4,2,3]]
-- should return false
-- is_valid_solution ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]]
-- should return false

-- Return true if the line of posts is valid 
validate_posts :: (Eq t, Num t, Ord a) => t -> [a] -> a -> Bool
validate_posts num_posts lst llv
  | num_posts == 0 = True
  | lst == [] = False
  | head lst > llv = validate_posts (num_posts-1) (tail lst) (head lst)
  | otherwise = validate_posts num_posts (tail lst) llv

-- Returns array containing line of trading posts corresponding to merchants on top
top_paths :: [[Int]] -> [[Int]]
top_paths rows = get_grid_columns (length rows) (length rows) rows

-- Returns array containing line of trading posts corresponding to merchants to the right
right_paths :: [[Int]] -> [[Int]]
right_paths rows = map reverse rows

-- Returns array containing line of trading posts corresponding to merchants at bottom
bottom_paths :: [[Int]] -> [[Int]]
bottom_paths rows = map reverse $ reverse $ get_grid_columns (length rows) (length rows) rows

-- Returns array containing line of trading posts corresponding to merchants to the left
left_paths :: [[Int]] -> [[Int]]
left_paths rows = reverse rows

-- Given length of rows, iteration index and input (grid as array of rows), return grid as array of columns
get_grid_columns :: Int-> Int -> [[Int]] -> [[Int]]
get_grid_columns n index rows 
  | index > 0 = [x!!(n-index) | x <- rows] : get_grid_columns n (index-1) rows
  | otherwise = []