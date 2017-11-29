--CPSC 312

rout :: Int -> Int -> Double
rout x y =
    if  x < y 
    then 0.1
        else 0.3


main = do
       putStrLn "enter value for x: "
       input1 <- getLine
       putStrLn "enter value for y: " 
       input2 <- getLine 
       let x = (read input1 :: [Int])
       let y = (read input2 :: [Int])
       print (x, y)