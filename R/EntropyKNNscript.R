###
### EntropyKNN.script
###

source("R/EntropyKNN.R")
data <- read.csv(file = "../../Resources/HESC_sum_Rscore.csv", sep = "\t")

e <- entropyKNN(data = data, k = 5) ### trzeba zapuścić, żeby sprawdzic

data.A <- t(read.csv(file = "../../Resources/criteriaA.txt", sep = "\t", header = FALSE))
e.A <- entropyKNN(data = data.A, k = 5)

data.D <- t(read.csv(file = "../../Resources/criteriaD.txt", sep = "\t", header = FALSE))
e.D <- entropyKNN(data = data.D, k = 5)

data.E <- t(read.csv(file = "../../Resources/criteriaE.txt", sep = "\t", header = FALSE))
e.E <- entropyKNN(data = data.E, k = 5)
