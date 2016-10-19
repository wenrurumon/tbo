
library(fda)
library(MASS)
fpca <- function(x,pos,nbasis=37){
	if(length(pos)<=3){
			return(NULL)
		}else{
			fbasis<-create.fourier.basis(c(0,1),nbasis=nbasis)
			fphi <- eval.basis(pos,fbasis)
			fcoef <- ginv(t(fphi)%*%fphi)%*%t(fphi)%*%t(x)
			coef<-t(fcoef-rowMeans(fcoef))/sqrt(nrow(x))
			scores<-coef%*%prcomp(coef)$rotation
	    	prop<-apply(scores,2,var)
	    	prop<-cumsum(prop/sum(prop))
	    	nscores <- which(prop>0.9999)[1]
	    	scores <- scores[,1:nscores,drop=F]
	    	prop <- prop[1:nscores]
	    	return(list(scores=scores,prop=prop))
   		}
}
fpca_smoothed <- function(x,pos,nbasis=37,lambda=0.1){
	if(length(pos)<=3){
			return(NULL)
		}else{
		  	fbasis<-create.fourier.basis(c(0,1),nbasis=nbasis)
			fphi <- eval.basis(pos,fbasis)+eval.basis(pos,fbasis,2)*lambda
			fcoef <- ginv(t(fphi)%*%fphi)%*%t(fphi)%*%t(x)
		  	coef<-t(fcoef-rowMeans(fcoef))/sqrt(nrow(x))
			scores<-coef%*%prcomp(coef)$rotation
		    prop<-apply(scores,2,var)
		    prop<-cumsum(prop/sum(prop))
		    nscores <- which(prop>0.9999)[1]
		    scores <- scores[,1:nscores,drop=F]
		    prop <- prop[1:nscores]
    		return(list(scores=scores,prop=prop))
    	}
}
fpca2 <- function(x,pos){
	list(x.fpca=fpca(x,pos,nbasis=length(pos)*2-1),
		x.sfpca=fpca_smoothed(x,pos,nbasis=length(pos)*2-1))
}
