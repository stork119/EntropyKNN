###
### KNN.ENTROPY
###

sourceCpp("src/source.cpp")

entropyKNN <- function(
  data,
  k = 5){

  data.M  <- data.matrix(data)
  ncol    <- ncol(data.M)
  nrow    <- nrow(data.M)
  kNNdist <- get_k_neighbour_2(
    data = data.M,
    k = k)

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
    rN[i]   <- kNNdist[i]^(1/ncol)/((nrow-1)*UnitBallVolume(ncol)*(-digamma(1)))^(1/ncol)
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

