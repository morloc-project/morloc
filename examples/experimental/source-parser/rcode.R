#-- runif :: Int -> Num -> Num -> [Num]

#-- sqrt :: [Num] -> [Num]
#-- sqrt :: Num -> Num

#-- rpois :: Int -> Num -> [Int]

#-- sum :: [Num] -> Num

#-- sumRmNa :: [Maybe Num] -> Num
sumRmNa <- function(x) sum(x, na.rm=TRUE)

library(MASS)

#-- mvrnorm :: Int   -> Array Num -> Matrix Num
#--         :: Count -> Mu        -> Sigma
