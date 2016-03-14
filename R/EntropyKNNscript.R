###
### EntropyKNN.script
###

source("R/EntropyKNN.R")
data <- read.csv(file = "../../Resources/HESC_sum_Rscore.csv", sep = "\t")

e <- entropyKNN(data = data[ sample(x = 1:nrow(data), size = 10000) , ], k = 5) ### trzeba zapuścić, żeby sprawdzic

# e.distr.1 <- entropyDistriubtion(data = data, n = 1000, k = 5), data.size = 1000)
# library(plyr)
# data2 <- data
# data2[2,] <- data[1,]
# 
# nrow(match_df(data.frame(data),data.frame(data2)))
# # dwa razy to samo + match < nrow < size wtedy jest gdzieś powtorzenie

hist(e.distr.1$entropy.permutation)


data <- matrix(c(rnorm(100,0,1),rnorm(100,2,4) + 10), ncol = 2, byrow = FALSE )

data.A <- t(read.csv(file = "../../Resources/criteriaA.txt", sep = "\t", header = FALSE))

e.A.distr.1 <- entropyDistriubtion(data = data, n = 1000, k = 5)
e.A.distr.1$entropy.pval

e.A <- entropyKNN(data = data.A, k = 5)

data.D <- t(read.csv(file = "../../Resources/criteriaD.txt", sep = "\t", header = FALSE))
e.D <- entropyKNN(data = data.D, k = 5)

data.E <- t(read.csv(file = "../../Resources/criteriaE.txt", sep = "\t", header = FALSE))
e.E <- entropyKNN(data = data.E, k = 5)
