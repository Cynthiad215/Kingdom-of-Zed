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
             if (dim == 2)
              then topfn dim tArr
              else print ("end")

topfn dim tArr = do
            putStrLn "enter values for the top index [T1, T2]: " 
            top1 <- getLine 
            let t = (read top1 :: Int)
            if (elem t [0..dim])
             then do
                  let tArr = [t]
                  putStrLn "enter values for the next top index [_, T2]: " 
                  top2 <- getLine 
                  let t2 = (read top2 :: Int)
                  if (elem t2 [0..dim])
                   then do 
                        let tArr = [t, t2]
                        print (tArr)
                   else do 
                        print ("bad input, start again")
                        topfn dim
             else do
                  print ("bad input, try again")
                  topfn dim
            print (tArr)

topfn2 dim = do
            putStrLn "enter values for the top index: " 
            top1 <- getLine 
            let t = (read top1 :: Int)
            if (elem t [0..dim])
             then do 
                  let tleft1 = t
                  putStrLn "enter values for the top index 2nd value: " 
                  top2 <- getLine 
                  let t2 = (read top2 :: Int)
                  if (elem t2 [0..dim])
                   then do 
                        let tleft2 = t2
                        print ("done")
                   else print ("hmm")
             else do 
             print ("oops that's not valid!") 
             topfn dim
            print ("done!")

