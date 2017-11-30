import Data.List

-- Generate all row permutations for gameboard of size n.
generate n = permutations [1..n]

-- Solve game board
solve (top, right, bottom, left) = top

-- Return true if the given board is a valid solution given input
is_valid_solution (top, right, bottom, left) input = True

-- Return true if the line of posts is valid 
is_valid_line num_posts lst llv
  | num_posts == 0 = True
  | lst == [] = False
  | (head lst) > llv = is_valid_line (num_posts-1) (tail lst) (head lst)
  | otherwise = is_valid_line num_posts (tail lst) llv

-- Returns array containing line of trading posts corresponding to merchants on top
filter_top_posts input = get_grid_columns (length input) (length input) input

-- Returns array containing line of trading posts corresponding to merchants to the right
filter_right_posts input = map reverse input

-- Returns array containing line of trading posts corresponding to merchants at bottom
filter_bottom_posts input = map reverse (reverse (get_grid_columns (length input) (length input) input))

-- Returns array containing line of trading posts corresponding to merchants to the left
filter_left_posts input = reverse input

-- Given length of rows, iteration index and input (grid as array of rows), return grid as array of columns
get_grid_columns n index rows 
  | index > 0 = [x!!(n-index) | x <- rows] : get_grid_columns n (index-1) rows
  | otherwise = []