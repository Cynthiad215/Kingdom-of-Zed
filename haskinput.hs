--CPSC 312

rout :: Int -> Int -> Double
rout x y =
    if  x < y 
    then 0.1
        else 0.3

--main = do
--       putStrLn "enter the dimension for your grid: "
--       input1 <- getLine
--       putStrLn "enter values for the top index, separated by a comma: " 
--       input2 <- getLine 
--       let dim = (read input1 :: Int)
--       let top = (read input2 :: [Int])
--       if (length top == dim)
--        then print ("true")
--        else print ("false")
--       print ("done!")

main = do
       putStrLn "enter the dimension for your grid: "
       dimension <- getLine
       let dim = (read dimension :: Int)
       if (dim > 4 || dim == 0)
        then do 
         print ("Timeout error, please try again")
         main
        else do 
         topfn dim


topfn dim = do
            putStrLn "enter values for the top index: " 
            top1 <- getLine 
            let t1 = (read top1 :: Int)
            if (elem t1 [0..dim])
             then print ("true")
             else do 
             print ("oops that's not valid!") 
             topfn dim
            print ("done!")