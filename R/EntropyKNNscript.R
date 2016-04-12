###
### EntropyKNN.script
###

source("R/EntropyKNN.R")

data <- read.csv(file = "data/HESC_sum_Rscore.csv", sep = "\t") #wczytaj dane
head(data)

# wybieram size = 10000 elementów z danych
e <- entropyKNN(data = data[ sample(x = 1:nrow(data), size = 10000) , ], k = 5) ### trzeba zapuścić, żeby sprawdzic

# wybieram cały zbiór
e <- entropyKNN(data = data, k = 5) ### trzeba zapuścić, żeby sprawdzic

e.distr <- entropyDistriubtion(data = data, data.size = 1000, n = 1000, k = 5)

hist(e.distr$entropy.permutation)