import Control.Monad

repeatNTimes 0 _ = return()
repeatNTimes n action =
 do
  action
  repeatNTimes(n-1) action

main = 
 do
  putStrLn "Dimension"
  input <- getLine
  let i = (read input :: Int)
  repeatNTimes i ( do
                    putStrLn "Numer"
                    input2 <- getLine
                    print (input2)
                    let arr = (read input2 :: Int) )
