
rm(list=ls())

#Cost Function

cost <- function(x,input=input){
  c1 <- input[1]; c2 <- input[2]; c3 <- input[3]; c4 <- input[4]; g <- input[5]
  abs(c1-c2/(1+x)-c3/(1+x)^2-c4*(1+g)/((x-g)*(1+x)^2))
}

#Genetic Programming
genetic_programming <- function(parents,itn=10,cost=cost){
  rlti <- sapply(parents,cost,input=input)
  # plot.ts(rlti)
  i <- 1
  while(i<itn){
    i <- i+1
    parents <- parents[which(rlti<=quantile(rlti,na.rm=TRUE)[2])]
    parents <- colMeans(rbind(sample(parents,size=length(parents)*2,replace=T),sample(parents,size=length(parents)*2,replace=T)))
    rlti <- sapply(parents,cost,input=input)
    costi <- min(rlti,na.rm=T)
    # print(costi)
    if(costi <= 1e-5){break}
  }
  return(
    c(x=parents[which(rlti==costi)[1]],
      cost=costi,
      itn=i
    ))
}

#dichotomy
dichotomy <- function(xmin, xmax, itn = 100, cost=cost){
  i <- 0
  while(i<itn){
    i <- i+1
    xmed <- (xmin+xmax)/2
    rlti <- sapply(c(xmin,xmax),cost,input=input)
    # print(min(rlti))
    if(rlti[1]>rlti[2]){
      xmin <- xmed
    } else {
      xmax <- xmed
    }
    if(xmin==xmax){break}
  }
  return(
    c(x=xmin,cost=cost(xmin,input),itn=i)
  )
}

#dichotomy tree
dt <- function(xmin,xmax,ntree=5,itn=100,cost=cost){
  xmin <- -5; xmax <- 5; ntree=5; itn=100;
  tree <- sort(c(xmin,xmax,runif(4,-5,5)))
  while(ntree>0){
    rlt.tree <- sapply(1:ntree,function(i){
      dichotomy(tree[i],tree[i+1],itn=itn,cost=cost)
    })
    tree <- sort(rlt.tree[1,])
    ntree <- length(tree)-1
  }
  return(rlt.tree[,1])
}

#Simulation

test.gp <- sapply(1:30,function(x){
  set.seed(x); input <<- runif(5)
  print(x)
  genetic_programming(-5000:5000/1000,itn=10,cost=cost)
})
test.dt <- sapply(1:30,function(x){
  set.seed(x); input <<- runif(5)
  print(x)
  dt(-5,5,ntree=5,itn=10,cost=cost)
})


#################

set.seed(123); input <- runif(5); print(input)
plot.ts(sapply(-5000:5000/100,cost,input=input))
genetic_programming(-5000:5000/1000,itn=30,cost=cost)
dt(-5,5,ntree=5,itn=100,cost=cost)
