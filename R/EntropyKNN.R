###
### KNN.ENTROPY
###
library(Rcpp)
library(foreach)
library(doParallel)

sourceCpp("src/source.cpp")

entropyKNN <- function(
  data,
  k = 5,
  knn_measure = knn_measure_fun(type = "max")){
  
  get_k_neighbour <- knn_measure$get_k_neighbour
  UnitBallVolume  <- knn_measure$UnitBallVolume
  entropy_fun     <- knn_measure$entropy_fun
  
  data.M  <- data.matrix(data)
  ncol    <- ncol(data.M)
  nrow    <- nrow(data.M)
  kNNdist <- do.call(get_k_neighbour,
                     list(
                       data = data.M,
                       k = k))


  lbound <- apply(data.M,2,min)
  ubound <- apply(data.M,2,max)
  rN     <- rep(0, times = nrow)
  vol    <- rep(0, times = nrow)
  prop   <- rep(0, times = nrow)

  unit_ball <- UnitBallVolume(ncol)
  for (i in 1:nrow){
    rN[i]   <- kNNdist[i]^(1/ncol)/((nrow-1)*unit_ball*(-digamma(1)))^(1/ncol) #dzielenie
    vol[i]  <- prod(pmin(rN[i], abs(data.M[i,]-lbound)) + pmin(rN[i], abs(data.M[i,]-ubound)))
    prop[i] <- vol[i]*(nrow-1)*(-digamma(1))/kNNdist[i]
  }
  
  entropy       <- -digamma(k) + digamma(nrow) + log(UnitBallVolume(ncol)) + ncol*sum(log(kNNdist))/nrow
  results       <- entropy_fun(list(entropy = entropy), prop, nrow)

  return(results)
}

entropyDistriubtion <- function(data,
                                data.size = nrow(data),
                                n = 1000,
                                k = 5,
                                knn_measure = knn_measure_fun(type = "max"),
                                threads_num = 4
){
  
  dataPermutation <- function(data){
    for(j in 1:ncol(data)){
      data[,j] <- sample(x = data[,j],
                         size = nrow(data),
                         replace = FALSE
      )
    }
    return(data)
  }
  
  
  cl <- makeCluster(threads_num)
  registerDoParallel(cl)
  
  entropy.permutation <- foreach(i = 1:n, .export = c('entropyKNN', 
                                                      'data')) %do%
  {
      data.tmp <- dataPermutation(data[sample(x = 1:nrow(data), size = data.size), ])
      e <- entropyKNN(data = data.tmp,
                      k = k,
                      knn_measure = knn_measure)
      return(e$entropy)
  }
  
  stopCluster(cl)
  
  entropy.permutation <- unlist(entropy.permutation)
  entropy <- entropyKNN(data = data[sample(x = 1:nrow(data), size = data.size), ],
                  k = k,
                  knn_measure = knn_measure)$entropy
  entropy.pval <- sum(entropy.permutation < entropy)/n
  return(
    list(entropy.pval = entropy.pval,
         entropy = entropy,
         entropy.permutation = entropy.permutation,
         k = k,
         n = n,
         data.size = data.size))
}

knn_measure_fun <- function(type = "max"){
  if(type == "max"){
    return(list(
      get_k_neighbour = get_k_neighbour_2,
           UnitBallVolume  = function(n)
           {
             return(2^n)
           },
           entropy_fun  = function(results, prop, nrow){
             results$entropy.corr  <- sum(log(prop))/nrow
             results$entropy.KNN.Charzynska <- results$entropy + results$entropy.corr
             return(results)
           })
    )
  } else if (type == "euclidean"){
    return(list(
      get_k_neighbour = get_k_neighbour_euclidean,
           UnitBallVolume  = function(n)
           { 
               if(n %% 2 == 0){
                  return(pi^(n/2)/factorial(n/2))
               } else {
                   return((2*factorial((n-1)/2)*((4*pi)^((n-1)/2)))/(factorial(n)))
               }
           },
           entropy_fun  = function(results, prop, nrow){
             return(results)
           })
    )
  } else {
    #warning
  }
}
