
############################
# Gradiant Feature
############################

g <- test
par(mfrow=c(1,2))
graphics::image(g,col=grey(0:64/64))
g.grad <- function(g,block=1){
  out <- matrix(0,nrow(g),ncol(g))
  for(i in (block+1):(ncol(g)-block)){
    for(j in (block+1):(ncol(g)-block)){
      out[i,j] <- max(abs(g[i,j]-g[-block:block+i,-block:block+j]))
    }
  }
  out
}
g2 <- g.grad(g)
graphics::image(g2,col=grey(0:64/64))
