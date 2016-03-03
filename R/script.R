#### Configuration ####
#install.packages("devtools")
#install.packages("data.table")
#install.packages("doParallel")
library(devtools)
library(dplyr)
library(parallel)
library(doParallel)
library(data.table)

amount <- max(data$.key)

cl.number <- 8
k <- 6
#### reading data ####
data.hesc <- read.csv(file = "../Resources/HESC_sum_Rscore.csv", sep = "\t")

data <- data.table(data.hesc)
data$.key <- 1:nrow(data)
setkey(data, .key)

i <- 1
data.nrow <- nrow(data)
data.ncol <- ncol(data)
data.colnames <- colnames(data)
#temp <- sapply(1:data.ncol, function(j){data[-i,j] - data[i, j]})

  di <- data.table(data = matrix(unlist(rep(x = data[i,], times = data.nrow - 1)) ,
                   nrow = data.nrow - 1,
                   ncol = data.ncol,
                   byrow = TRUE))

  dii <- abs(data[-i,] - di)


time.1.1 <-system.time({
#  cl <- makeCluster(cl.number);
  data.list.1.1 <- list()
  for(i in data$.key[1:amount]){
    data.div <- abs(data -
          data[rep(x     = i,
                   times = data.nrow),])
    data.div$.max <- apply(data.div, 1, max)
    data.list.1.1[[i]] <- data.div[-i,][order(.max)][k]$.max
  };
#stopCluster(cl)
})

time.1.2 <-system.time({
  cl <- makeCluster(cl.number);

  data.list.1.2 <- foreach(i = data$.key[1:amount], .packages = "data.table") %dopar% {
    data.div <- abs(data -
                      data[rep(x     = i,
                               times = data.nrow),])
    data.div$.max <- apply(data.div, 1, max)
    return(data.div[-i,][order(.max)][k]$.max)
  };
  stopCluster(cl)
})

time.2.1 <- system.time({

  temp1<-matrix(0, nrow=N-1, ncol=dimY)
  #cl <- makeCluster(cl.number);
  data.list.2.1 <- foreach (i = 1:amount) %do% {
    temp<-matrix(0, nrow=N-1, ncol=1)
    for (d in 1:dimY){
      # temp<-temp+(Y[-i,d]-Y[i,d])^2;     # Euclidian norm
      temp1[,d]<-abs(Y[-i,d]-Y[i,d]);      # max norm
    }
    #       rep.row<-function(x,n){
    #         matrix(rep(x,each=n),nrow=n)
    #       }
    #       temp1[,]<-abs(Y[-i,]-rep.row(Y[i,],N-1))

    # # pY[i]<-1/sqrt(min(temp))           # nn Euclidian norm
    # temp<-sort(temp)
    # pY(i)=1/sqrt(temp(k))
    temp<-apply(temp1,1,max)               # nn max norm
    temp<-sort(temp)
    return(temp[k])                    # k-nn max norm
  }
  #stopCluster(cl)
})

time.2.2 <- system.time({

  temp1<-matrix(0, nrow=N-1, ncol=dimY)
  cl <- makeCluster(cl.number);
  data.list.2.1 <- foreach (i = 1:amount) %dopar% {
    temp<-matrix(0, nrow=N-1, ncol=1)
    for (d in 1:dimY){
      # temp<-temp+(Y[-i,d]-Y[i,d])^2;     # Euclidian norm
      temp1[,d]<-abs(Y[-i,d]-Y[i,d]);      # max norm
    }
    #       rep.row<-function(x,n){
    #         matrix(rep(x,each=n),nrow=n)
    #       }
    #       temp1[,]<-abs(Y[-i,]-rep.row(Y[i,],N-1))

    # # pY[i]<-1/sqrt(min(temp))           # nn Euclidian norm
    # temp<-sort(temp)
    # pY(i)=1/sqrt(temp(k))
    temp<-apply(temp1,1,max)               # nn max norm
    temp<-sort(temp)
    return(temp[k])                    # k-nn max norm
  }
  stopCluster(cl)
})


#### data analysis ####
# 1. Check if data is numerical
# 2. Check if data consist

####


Y <- data.hesc

#EMc=0.5772;                    # Euler-Masheroni constant psi(k) <- digamma function
N<-dim(as.matrix(Y))[1]
dimY<-dim(as.matrix(Y))[2]
k=1                            # k-NN
kNNdist<-vector("numeric",N)              #m kNNdist=zeros(1,N);

# #   %     function V=UnitBallVolume(n)               % Euclidian norm
# #   %       if mod(n,2)==0;
# #   %           V=pi^(n/2)/factorial(n/2);
# #   %       else
# #     %           V=(2^((n+1)/2)*pi^((n-1)/2)/prod(1:2:n)); % podwojna silnia
# #   %       end
# #   %     end
#
# #   function V=UnitBallVolume(n)               % max norm
# #   V=2^n;
# #   end
#
# UnitBallVolume<-function(n)
# {
#   V=2^n
#   return(V)
# }
#
#
#
# #  %  % correction
# lbound <-apply(as.matrix(Y),2,min)
# ubound <-apply(as.matrix(Y),2,max)
# rN<-matrix(0, nrow=1, ncol=N)
# vol<-matrix(0, nrow=1, ncol=N)
# prop<-matrix(0, nrow=1, ncol=N)
#
# for (i in 1:N){
#   rN[i]=kNNdist[i]^(1/dimY)/((N-1)*UnitBallVolume(dimY)*(-digamma(1)))^(1/dimY)
#   vol[i]=prod(pmin(rN[i],abs(as.matrix(Y)[i,]-lbound))+pmin(rN[i],abs(as.matrix(Y)[i,]-ubound)))
#   prop[i]=vol[i]*(N-1)*(-digamma(1))/kNNdist[i]
# }
# cr=sum(log(prop))/N
#
# #   %     % estymator entropii Perez-Cruz
# #   %     pY=pY.^dimY/(UnitBallVolume(dimY)*(N-1));
# #   %     estHY=-sum(log(pY))/N;
# #   %     CestHY=estHY+EMc;
# #
# #   %       % estymator entropii Kraskov
# #   %       estHY=-dimY*sum(log(pY))/N;                             % Euclidian norm
# #   %       CestHY=-psi(k)+psi(N)+log(UnitBallVolume(dimY))+estHY;
#
# #  % max norm
# estHY<-dimY*sum(log(kNNdist))/N
# CestHY=-digamma(k)+digamma(N)+log(UnitBallVolume(dimY))+estHY+cr
#
# return(CestHY)
