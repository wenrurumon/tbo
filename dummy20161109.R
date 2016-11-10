

#create dummy data
X <- data.frame(
  v1 = rnorm(100),
  v2 = rnorm(100),
  v3 = rnorm(100),
  v4 = letters[sample(1:26,100,T)],
  v5 = letters[sample(1:26,100,T)]
)
X[sample(1:prod(dim(X)),size=40)] <- NA
head(X)

#FUNCTION, return different output in case of the data type of column i.
fun <- function(i){
  x <- X[,i]
  if(is.numeric(x)){
    return(
      c(
        Attribute_ID=i,Attribute_Name=colnames(X)[i],Missing=sum(is.na(x)),
        Mean=mean(x,na.rm=T),Median=median(x,na.rm=T),Sdev=sd(x,na.rm=T),Min=min(x,na.rm=T),Max=max(x,na.rm=T)
        )
    )
  } else {
    return(
      c(
        Attribute_ID=i,Attribute_Name=colnames(X)[i],Missing=sum(is.na(x))
      )
    )
  }
}

#run the fun function by column
rlt <- lapply(1:ncol(X),fun)

#output
print(paste('this datraset has',nrow(X),'Rows and',ncol(X),'Cols'))
print(do.call(rbind,rlt[sapply(rlt,length)==8]))
print(do.call(rbind,rlt[sapply(rlt,length)==3]))
