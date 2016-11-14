#file name is  "SDR.class.r"

#Description:
#x: is the data n by p
#y: is the response variable, here is for binary coding, 1,2 (or 1,0)
#d: is the dimention
#rho:
#lambda: 0.0001
#r: 0
#interatrion: 10000

library(MASS)
library(Matrix)

class_SDR <- function(x, y, rho=1, delta=1, lambda=0.0001, r=0, iteration=1000, max.diff = 10e-5)
{
	y.value <- unique(y)
	y.class <- length(unique(y))

	#make a indicator matrix
	yy <- matrix(0, nrow=length(y), ncol=y.class)

	for (nClass in 1:y.class )
	{
		for (j in 1:length(y))
		{
			if(y[j] == y.value[nClass])
			{
				yy[j,nClass] <- 1
			}
		}
	}

	x <- data.matrix(x)
	y <- data.matrix(yy)

	d <- dim(y)[2]

################################
	num.col <- ncol(x)
	num.row <- nrow(x)

	stdize <- function(v) (v - mean(v))/sd(v)

# norm of a vector
	norm<-function(v)  
	{	 
		sumv2<-sum(v^2)
		sqrt(sumv2)
	}

	y.n <- nrow(y);  

	y.center <- apply(y,2,stdize)

	d.pi <- NULL;
	d.pi <- (t(y) %*% y) *(1/y.n);
	d.pi.inv <- solve(d.pi);
  
	Q <- matrix(0, nrow=nrow(d.pi), ncol=d);  
	x.center <- apply(x,2,stdize)
##################################
	xz <- t(x.center) %*% y.center
	
	if(num.col > num.row)
	{
		xx_t <- x.center %*% t(x.center)
		i_xx <- diag(dim(xx_t)[1])
		
		ixx.inv <- solve(-i_xx - xx_t*2/rho)

		#x_inv <- (2/rho *t(x.center) %*% ixx.inv %*% x.center + diag(dim(x.center)[2])) *2 /rho
		
		x.ixx.inv <- t(x.center) %*% ixx.inv *4 /rho^2			
	 }	else 
	 {
		 xx_tt <- t(x.center) %*% x.center
		
		 i_xxt <- diag(dim(xx_tt)[1])
	
		 ixx2.inv <- solve(i_xxt*rho/2 + xx_tt)		 
	 }
	
  #initial theta
	q1 <- as.vector(rep(1,nrow(d.pi)));  
	rnd_fun <- rnorm;

  #initial the beta and theta
	Q <- NULL;
	theta_ini <-NULL;
	theta_ini_i <-NULL;
	beta_ini <- NULL;
	beta_ini_i <- NULL;
	Q_update <- NULL;

    for ( ii in 1:d)
    {
      set.seed(123456);
	  rnd_args <- list(mean=0, sd=1);
      rnd_vec <- do.call(rnd_fun, c(list(n=nrow(d.pi)), rnd_args)) * 0.0001;
      I_matrix <- diag(nrow(d.pi));

	  if (ii == 1)
      {Q <- cbind(Q,q1);}

      theta_ini_i <- (I_matrix - Q %*% t(Q) %*% d.pi) %*% rnd_vec ;
	  
	  if(sum(theta_ini_i) >= 0.001 && sd(theta_ini_i) !=0)
	  {	  
		theta_ini_i <- sqrt(1/(t(theta_ini_i) %*% d.pi %*% theta_ini_i))[1,1] * theta_ini_i;  
	  }
	  else if (sum(theta_ini_i) == 0)
	  {
		theta_ini_i <- sqrt(1/(t(as.matrix(rnd_vec)) %*% d.pi %*% rnd_vec))[1,1] * as.matrix(rnd_vec);  
	  }
	  else if (sd(theta_ini_i) == 0 )
	  {
		theta_ini_i <- sqrt(1/(t(as.matrix(rnd_vec)) %*% d.pi %*% rnd_vec))[1,1] * as.matrix(rnd_vec);  
      }	
      
	  theta_ini <- cbind(theta_ini, theta_ini_i)
      	  
	  # if(num.col > num.row)
	  # { 
		# beta_ini_i <- 2/rho * xz %*% theta_ini_i + x.ixx.inv %*% (x.center %*% (xz %*% theta_ini_i));
				
	  # }else
	  # {
		# beta_ini_i <- ixx2.inv %*% xz  %*% theta_ini_i
	  # }	
	  
	  beta_ini_i <- matrix(0,dim(x.center)[2],1)
		
	  beta_ini <- cbind(beta_ini, beta_ini_i);

      if(ii<d) {Q <- cbind(Q, theta_ini_i)}
	  
    }

	alpha_ini <- beta_ini;
	mu_ini <- matrix(0,ncol=d,nrow=nrow(beta_ini));
  
	beta <- beta_hat <- beta_ini;
	alpha <- alpha_hat <- alpha_ini;
	mu <- mu_hat <- mu_ini;
	theta <- theta_hat <- theta_ini;
  
	I_matrix_beta <- diag(nrow(d.pi));
  
	y.center <- data.matrix(y.center)
	n <- nrow(y.center)

	lamb.delda.r <- 1 + 2*lambda *(1-delta)*(1+r)/rho
	lamb.delda.r.rho <- lambda * delta *(1-r^2) /rho

    for (i in 1:iteration)
    {
      #set the initial values and update them
      beta_hat  <- beta;
      alpha_hat <- alpha;
      mu_hat    <-mu;    
      theta_hat <- theta
      Q_update  <-Q
    
      #update beta with fixed theta
      for (j in 1:d)
        {        
        if(num.col > num.row)
			{
				beta[,j] <- 2/rho * xz %*% theta_hat[,j] + (alpha_hat[,j] - mu_hat[,j]) + x.ixx.inv %*% (x.center %*% (xz %*% theta_hat[,j] + (alpha_hat[,j] - mu_hat[,j]) * rho /2 ))
			}
			else if (num.col <= num.row)
			{
				beta[,j] <- ixx2.inv %*% (xz %*% theta_hat[,j] + (alpha_hat[,j] - mu_hat[,j])* rho/2  )
			}
		}

      beta_star <- t(beta);
      alpha_star <- t(alpha_hat);
      mu_star <- t(mu_hat);

      ncol.alpha_star <- ncol(alpha_star);
      
      beta_mu_out<- NULL;
    
      for (k in 1:ncol.alpha_star)
      { 
        ###############CHANGE 20130118##################
        norm_BM <- (sqrt(t(beta_star[,k] + mu_star[,k]) %*% (beta_star[,k] + mu_star[,k])))**(1+r)
        ################################################
        
		beta_mu <- norm_BM - lamb.delda.r.rho 
        
         if (is.na(beta_mu))
		 {beta_mu <- 0}
		 else if (beta_mu <= 0)
		 {beta_mu <- 0}
		 else if (beta_mu > 0)
         {beta_mu <- (beta_mu/lamb.delda.r)^(1/(1+r))} 
		
        beta_mu_out<- rbind(beta_mu_out, beta_mu)
      
        alpha_star[,k] <- beta_mu * (beta_star[,k] + mu_star[,k]) / norm_BM;
      }

      alpha <- t(alpha_star);
    
      for (s in 1:d)
      {
        mu[,s] <- mu_hat[,s] + beta[,s] - alpha[,s]
      }

##########################################
#calculate theta for fixing beta
####Newton method to solve theta
################################################################################
      
          for (m in 1:d)
      {
        QQ <- Q_update[,1:m] %*% t(Q_update[,1:m]);
                #theta_hat is a number
        theta_YXB <- n * theta_hat[,m] %*% t(y.center) %*% x.center %*% beta[,m]    
        I_QQ <- (I_matrix_beta - QQ %*% d.pi) %*% d.pi.inv %*% t(y.center) %*% x.center %*% beta[,m]
        TYXB <- theta_YXB + t(beta[,m]) %*% t(x.center) %*% y.center %*% theta_hat[,m]
        TYXB_I_QQ <- theta_YXB[1] * theta_hat[,m] - I_QQ 
        theta[,m] <- theta_hat[,m] - (1/TYXB)[1] * TYXB_I_QQ
        
        #################changed here
        theta[,m] <- theta[,m] / sqrt(t(theta[,m]) %*% d.pi %*% theta[,m]); 
      
      ######CHANGE 20130118        
        if (m >1 )
        {
        Q_update[,m] <- theta[,m-1];
      ################################
        }           
      }
      ######CHANGE 20130118
      Q <- Q_update;
      #######################
################################################################################
      diff_theta <- theta_hat - theta;
      norm_theta_diff <- 0;
          
          diff_beta <- beta_hat - beta;
      norm_beta_diff <- 0;

      for (ll in 1:ncol(diff_theta))
      {
        norm_diff_th <- sqrt(t(diff_theta[,ll]) %*% diff_theta[,ll]);
        norm_theta_diff <- norm_theta_diff + norm_diff_th;
      }
          
          for (kk in 1:ncol(diff_beta))
      {
        norm_diff_b <- sqrt(t(diff_beta[,kk]) %*% diff_beta[,kk]);
        norm_beta_diff <- norm_beta_diff + norm_diff_b;
      }
       
	  cat("accuracy","=",i, "bete_diff", norm_beta_diff,"\n")
	  cat("beta_mu","=",i, "beta_mu", beta_mu,"\n")
	  		
      if (norm_theta_diff < max.diff & norm_beta_diff < max.diff)
      {
        break;
      }
  }
  
  beta <- data.matrix(beta)  
  rownames(beta) <- colnames(x)
   
  #out <- list("theta"=theta,"y.center"=y.center,"x"=x, "beta"=beta) 
  #return(out)
  return(beta)    
}
