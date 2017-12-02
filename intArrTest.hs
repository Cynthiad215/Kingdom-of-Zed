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
              then topfn dim
              else print ("end")

topfn dim = do
            putStrLn "enter values for the top line [T1,T2]: " 
            top <- getLine 
            let t1 = (read top :: [Int])
            putStrLn "enter values for the top line [R1,R2]: " 
            right <- getLine 
            let r1 = (read right :: [Int])
            putStrLn "enter values for the top line [B1,B2]: " 
            bottom <- getLine 
            let b1 = (read bottom :: [Int])
            putStrLn "enter values for the top line [L1,L2]: " 
            left <- getLine 
            let l1 = (read left :: [Int])
            print (t1, r1, b1, l1)
          
