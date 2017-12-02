import Solver
import System.Console.ANSI
import System.Exit
      
-- Start program
start :: IO ()
start = do
  userInput <- gatherUserInput
  let grid = solve userInput
  printAsGrid grid

-- Gather all user input
gatherUserInput :: IO [[Int]]
gatherUserInput = do
  putStrLn "Please specify a size (between 2 and 4) for the grid."
  gridSize <- getLine
  let dim = (read gridSize :: Int)
  if (elem dim [2..4])
    then do
      merchants <- getMerchants dim
      return merchants
    else
      die "You entered an invalid grid size."

-- Get merchant information given grid dimensions
getMerchants :: Int -> IO [[Int]]
getMerchants dim = do
  tm <- getTopMerchants dim
  rm <- getRightMerchants dim
  bm <- getBottomMerchants dim
  lm <- getLeftMerchants dim
  return [tm, rm, bm, lm]

-- Get top merchant information
getTopMerchants :: Int -> IO [Int]
getTopMerchants dim = do
  putStrLn "Please enter merchant visit values for the northern merchants, left to right." 
  putStrLn "Enter values as integers in an array like so: [x1,x2,...,xn]"
  putStrLn "Valid values range from 0 (incomplete info) to the size of the grid specified earlier."
  input <- getLine 
  let merchantInfo = (read input :: [Int])
  if (isValid merchantInfo dim)
    then
      return merchantInfo
    else
      die "You entered an invalid value. Only values between 0 and grid size inclusive are allowed."

-- Get right merchant information
getRightMerchants :: Int -> IO [Int]
getRightMerchants dim = do
  putStrLn "Please enter merchant visit values for the eastern merchants, top to bottom."
  input <- getLine 
  let merchantInfo = (read input :: [Int])
  if (isValid merchantInfo dim)
    then
      return merchantInfo
    else
      die "You entered an invalid value. Only values between 0 and grid size inclusive are allowed."

-- Get bottom merchant information
getBottomMerchants :: Int -> IO [Int]
getBottomMerchants dim = do
  putStrLn "Please enter merchant visit values for the southern merchants, right to left."
  input <- getLine 
  let merchantInfo = (read input :: [Int])
  if (isValid merchantInfo dim)
    then
      return merchantInfo
    else
      die "You entered an invalid value. Only values between 0 and grid size inclusive are allowed."

-- Get left merchant information
getLeftMerchants :: Int -> IO [Int]
getLeftMerchants dim = do
  putStrLn "Please enter merchant visit values for the western merchants, bottom to top."
  input <- getLine 
  let merchantInfo = (read input :: [Int])
  if (isValid merchantInfo dim)
    then
      return merchantInfo
    else
      die "You entered an invalid value. Only values between 0 and grid size inclusive are allowed."


-- Checks input arrays to ensure they have correct input
isValid :: (Eq t, Num t, Enum t) => [t] -> t -> Bool
isValid [] _ = True;
isValid (h:t) x = if (elem h [0..x]) then isValid t x else False

-- Prints array in a grid like representation
printAsGrid :: Show a => Maybe [a] -> IO ()
printAsGrid Nothing = do
  putStrLn "There was no solution given the specified information."
printAsGrid (Just grid) = do
  putStrLn "The solution to the specified map is as follows:"
  putStr $ unlines $ map show grid