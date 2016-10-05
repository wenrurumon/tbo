
rm(list=ls())
library(oro.dicom)
library(oro.nifti)

###################################################################################################
# Import data
###################################################################################################

loc <- 'C:\\Users\\zhu2\\Documents\\Lung_CT_CHINA\\Lung_CT_CHINA\\raw\\' 
setwd(loc)
d <- dir()
f <- function(di){
  setwd(loc)
  setwd(di)
  print(di)
  data.dicom <- readDICOM(getwd())
  data.nifti <- dicom2nifti(data.dicom)
  return(data.nifti)
}

raw <- list()
for(i in 1:10){
  di <- d[i]
  print(Sys.time())
  raw[[i]] <- try(f(di))
}

setwd('C:\\Users\\zhu2\\Documents\\Lung_CT_CHINA\\Lung_CT_CHINA\\raw\\')
save(raw,file='test_nifti.rda')

###################################################################################################
# Process nifti
###################################################################################################
#Macro
##################

rm(list=ls())
setwd('C:\\Users\\zhu2\\Documents\\Lung_CT_CHINA\\Lung_CT_CHINA\\')
load('test_nifti.rda')
library(graphics)
library(oro.dicom)
library(oro.nifti)
library(RNiftyReg)

plotg <- function(g,col=grey(0:64/64)){graphics::image(t(g),col=col)}
plotgs <- function(gs,d=1,is=dim(gs)[d]/(9:1)){
  par(mfrow=c(3,3))
  for(i in is){
    if(d==1){
      plotg(gs[i,,])
    }else if(d==2){
      plotg(gs[,i,])        
    } else if(d==3){
      plotg(gs[,,i])        
    }}
  par(mfrow=c(1,1))
}
g.var <- function(x){var(as.vector(x))}
g.grad <- function(g,block=1){
  out <- matrix(0,nrow(g),ncol(g))
  for(i in (block+1):(ncol(g)-block)){
    for(j in (block+1):(ncol(g)-block)){
      out[i,j] <- max(abs(g[i,j]-g[-block:block+i,-block:block+j]))
    }
  }
  return(out)
}
gs.grad <- function(gs){
  for(i in 1:dim(gs)[3]){
    gs[,,i] <- g.grad(gs[,,i])
  }
}
g.2group <- function(g,grp=2,value=T){
  g2 <- kmeans(as.vector(g),range(as.vector(g)))$cluster
  if(value){
    g2 <- matrix(ifelse(g2==grp,as.vector(g),0),nrow(g),ncol(g))
  }else{
    g2 <- matrix(ifelse(g2==grp,1,0),nrow(g),ncol(g))
  }
  g2/max(g)
}

g.align <- function(source,target){
  temp <- niftyreg.linear(source=source,target=target)
  temp <- niftyreg.nonlinear(source=temp$img,target=target)
  temp
}

g.process <- function(g){
  g <- g.2group(g,value=T)
  g <- g/max(g)
  g2 <- g.2group(g,value=F)
  colsel <- range(which(colMeans(g2)>0.5))
  rowsel <- range(which(rowMeans(g2)>0.3))
  for(i in 1:512){
    g[i,1:max(which(g2[i,1:colsel[1]]==0))] <- 0
    g[i,(colsel[2]-1+min(which(g2[i,colsel[2]:512]==0))):512] <- 0
  }
  list(graph=g,window=c(range(which(colSums(g)>0)),rowsel))
}

gs.process <- function(gs){
  gs2 <- lapply(1:dim(gs)[3],function(i){g.process(gs[,,i])})
  out <- array(0,dim(gs))
  for(i in 1:dim(gs)[3]){out[,,i] <- gs2[[i]]$graph}
  out <- NIfTI_parameter(as.nifti(out),gs)
  return(out)
}

NIfTI_parameter <- function(target,template){
  pixdim(target) <- pixdim(template);
  qform_code(target) <- qform_code(template);
  qoffset_x(target) <- qoffset_x(template);
  qoffset_y(target) <- qoffset_y(template);
  qoffset_z(target) <- qoffset_z(template);
  
  quatern_b(target) <- quatern_b(template);
  quatern_c(target) <- quatern_c(template);
  quatern_d(target) <- quatern_d(template);
  
  srow_x(target) <- srow_x(template);
  srow_y(target) <- srow_y(template);
  srow_z(target) <- srow_z(template);
  
  xyzt_units(target) <- xyzt_units(template);
  return(target);
}

kicksec <- function(gs){
  gs.var <- apply(gs,3,function(x){var(as.vector(x))})
  gs.new <- gs[,,gs.var!=0,drop=F]
  return(list(var=gs.var,image=gs.new))
}

##################
#Reg
##################

g.source <- gs.process(raw[[1]])
g.target <- gs.process(raw[[10]])
system.time(reg1 <- niftyreg(g.source,g.target,scope='rigid'))
system.time(reg2 <- applyTransform(forward(reg1),g.source))
system.time(reg3 <- niftyreg(reg2,g.target,scope='affine'))
plotgs(reg3$image,d=2,is=1:9 * 10+150)

