###
### KNN.ENTROPY
###
library(Rcpp)
library(foreach)

sourceCpp("src/source.cpp")

entropyKNN <- function(
  data,
  k = 5,
  get_k_neighbour = get_k_neighbour_2){

  data.M  <- data.matrix(data)
  ncol    <- ncol(data.M)
  nrow    <- nrow(data.M)
  kNNdist <- do.call(get_k_neighbour,
                     list(
                       data = data.M,
                       k = k))

  UnitBallVolume<-function(n)
  {
    V=2^n
    return(V)
  }

  lbound <- apply(data.M,2,min)
  ubound <- apply(data.M,2,max)
  rN     <- rep(0, times = nrow)
  vol    <- rep(0, times = nrow)
  prop   <- rep(0, times = nrow)

  for (i in 1:nrow){
    rN[i]   <- kNNdist[i]^(1/ncol)/((nrow-1)*UnitBallVolume(ncol)*(-digamma(1)))^(1/ncol) #dzielenie
    vol[i]  <- prod(pmin(rN[i], abs(data.M[i,]-lbound)) + pmin(rN[i], abs(data.M[i,]-ubound)))
    prop[i] <- vol[i]*(nrow-1)*(-digamma(1))/kNNdist[i]
  }
  entropy.corr  <- sum(log(prop))/nrow
  entropy       <- ncol*sum(log(kNNdist))/nrow
  entropy.KNN.Kraskov <- -digamma(k) + digamma(nrow) + log(UnitBallVolume(ncol)) + entropy
  entropy.KNN.Charzynska <- entropy.KNN.Kraskov + entropy.corr

  return(list(entropy.corr = entropy.corr,
              entropy = entropy,
              entropy.KNN.Kraskov = entropy.KNN.Kraskov,
              entropy.KNN.Charzynska = entropy.KNN.Charzynska))
}

entropyDistriubtion <- function(data,
                                data.size = nrow(data),
                                n = 1000,
                                k = 5,
                                get_k_neighbour = get_k_neighbour_2
){
  
  entropy.permutation <- foreach(i = 1:n) %dopar%
  {
      data.tmp <- dataPermutation(data[sample(x = 1:nrow(data), size = data.size), ])
      e <- entropyKNN(data = data.tmp,
        k = k,
        get_k_neighbour = get_k_neighbour)
      return(e$entropy.KNN.Charzynska)
  }
  entropy.permutation <- unlist(entropy.permutation)
  entropy <- entropyKNN(data = data[sample(x = 1:nrow(data), size = data.size), ],
                  k = k,
                  get_k_neighbour = get_k_neighbour)$entropy.KNN.Charzynska
  entropy.pval <- sum(entropy.permutation < entropy)/n
  return(list(entropy.pval = entropy.pval, entropy = entropy, entropy.permutation = entropy.permutation, k = k , n = n, data.size = data.size))
}

dataPermutation <- function(data){
  for(j in 1:ncol(data)){
    data[,j] <- sample(x = data[,j],
                       size = nrow(data),
                       replace = FALSE
                       )
  }
  return(data)
}

