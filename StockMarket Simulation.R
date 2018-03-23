

#-FUNCTION-# To calculate Mue and Sigma 
calculate.Mu.Sigma <- function (dataframe) {
  #Extracting the close price only
  s <- as.numeric(gsub(",","", dataframe$Close))
  
  #Setting up a variable to be used for storage
  Upar <- 0
  #Calculating LN(Si+1/Si)
  for (j in 1:(length(s) - 1)) {
    Upar[j] <- log(s[j] / s[(j + 1)])
  }# End Loop
  
  #Calculating Ubar
  Ubar <- mean(Upar)
  #Setting up a variable to be used for storage
  Ss <- 0
  #Calculating (LN(Si+1/Si) - Ubar)^2
  for (z in 1:length(Upar)) {
    Ss[z] <- (Upar[z] - Ubar) ^ 2
  }
  #Calculating S^2
  s2 <- mean(Ss)
  #Calculating Mu, Simgma
  Mu <- mean(c(s2,Ubar)) / (1 / (365*5.5*60))
  Sigma <- sqrt(s2 / (1 / (365*5.5*60)))
  
  return(list(Mu,Sigma))
}# End Function

######Stock Price S0 -> St  Function (normal distribution) ##########
St = function (S0,mu, sigma,t) {
  #Declaring Variables
  trials<- t *5.5*60  # One day trading per minutes
  trials = trials + 1
  f <- S <- rep(0, times = trials)
  S[1] <- S0;
  dt <- t / (trials - 1)
  for (i in 2:trials) {
    if (S[i] >= (S0 *1.1)){
      S[i:trials] <- S[i];
      next;
    }else{
      # Represents the stochastic portion
      f[i] <- f[i - 1] + sqrt(dt) * rnorm(1)
      # Now calculate the deterministic with the stochastic
      S[i] <-  S0 * exp((mu - (sigma ^ 2) / 2) * (i - 1) * dt + sigma * f[i])
    }# End IF
  } # End loop
  return(S)
} # End function  

###### TASI Index simulation Function per minutes #######
TSI_Exp_r = function (M0,muM,t) {
  #Declaring Variables
  
  trials<- t *5.5*60  # one day trading per minutes
  trials = trials + 1
  g<-  rep(0, times = trials)
  g[1]<- M0;
  dt <- t / (trials - 1)
  for (i in 2:trials) {
    #This represents the expected value  
    if(g[i] >= (M0 *1.1)){
      g[i] <- g[i];
      next;
    }else {
      g[i] <- g[1]*exp(muM*(i-1)*dt)
    }
  } # End loop
  return(g)
} # End function  

