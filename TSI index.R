
library("rvest")
#Function to extract Data ---- # Only modify the "First_Junk"
tasi_index <-
  function (from_year,from_month,from_day,to_year,to_month,to_day,page_number) {
    first_junk <-
      "http://www.tadawul.com.sa/wps/portal/!ut/p/c1/lYtBC8IgHEc_0v-HOqOzxbKGyDSZXsJDxahtEVFfP7t1qYh3fO9RosKY7_0x3_ppzGfqKMmdgQqtFgy1sXPojVxqvWgBL4uPbx6uArRogldNzQH8ddc2VK_bMWfXHJr9uF2-kllNw54ipdnHkgvyFFWJ4tfoMmy7hzmcnnx6O5I!/dl2/d1/L0lHSkovd0RNQUprQUVnQSEhL1lCWncvZW4!/?current_page=%2Ftasi%2Fjsp%2Fhtml%2Findeces_performance.&chart_tasi_current_sector=TASI&TASIactionString=chart_tasi.form.config_change&performance_tasi_fromdate="
    url_text <-
      paste(
        first_junk, from_year, "%2F",from_month,"%2F",from_day,"&performance_tasi_fromdate=",to_year,"%2F",to_month,"%2F",to_day,"&performance_tasi_fromdate_Month=11&performance_tasi_todate=",to_year,"%2F",to_month,"%2F",to_day,"&performance_tasi_todate=",to_year,"%2F",to_month,"%2F",to_day,"&performance_tasi_todate_Month=11&page_index_number=",page_number,sep = ""
      )
    
    html_page <- read_html(url_text)
    table <- html_nodes(html_page,".Table3 td")
    x <- html_text(table)

    #Organizing the data in data.frame format
    tablized <- data.frame()
    n <- 9
    for (r in 2:(length(x) / 8)) {
      for (c in 1:8) {
        tablized[r,c] <- x[n]
        n <- n + 1
      }# End Loop
    }# End Loop
    
    #Setting up the column names
    colnames(tablized) <- c(x[1:8])
    return (tablized[-1,]) # returning the data frame
  }# End Function

stock_data <-
  function (from_year,from_month,from_day,to_year,to_month,to_day,stock_symbol,page_number) {
    first_junk <-
      "http://www.tadawul.com.sa/wps/portal/!ut/p/c1/lYuxDoIwGAYf6f8oVMIoDFgkDVpoaBfTAQ1RioPR17dsLGrMjXdHlgLePceLe4yzdzfqyW5OEoU-ioShjLIIYq8OWce2DFUavFn7RnOIpFZMNVUMwf66oTiWW7dFXcYAftxyN08DqcGTIZt-LHNOLZk8ROZrdJ-6_iXP1zff3HzF/dl2/d1/L3dJMjJ3QSEhL3dIRUJGUUJndFFCZ0RRQmdyUUJoSFFCZ0FBISEvWUk1dy83X04wQ1ZSSTQyMEcxOTEwSUtTUTlVMkEyMEI1/?symbol="
    url_text <-
      paste(
        first_junk,stock_symbol,"&tabOrder=2&isNonAdjusted=0&resultPageOrder=",page_number,"&totalPagingCount=-1&firstinput=", from_year, "%2F",from_month,"%2F",from_day,"+&firstinput=",to_year,"%2F",to_month,"%2F",to_day,"&firstinput_Month=11&secondinput=",to_year,"%2F",to_month,"%2F",to_day,"+&secondinput=",to_year,"%2F",to_month,"%2F",to_day,"&secondinput_Month=11&si.x=35&si.y=11",sep = ""
      )

    html_page <- read_html(url_text)
    table <- html_nodes(html_page,".Table3 td.calibri-12")
    x <- html_text(table)
    a <- gsub("\t","", x)
    b<- gsub("\n","", a)
    x<- gsub("\r","", b)
    
    #Organizing the datain data.frame format
    tablized <- data.frame()
    n <- 11
    for (r in 2:(length(x) / 10)) {
      for (c in 1:10) {
        tablized[r,c] <- x[n]
        n <- n + 1
      }# End Loop
    }# End Loop
    
    #Setting up the column names
    colnames(tablized) <- c(x[1:10])
    return (na.omit(tablized[-1,])) # returning the data frame
  }# End Function



#-FUNCTION-# To Get all data in one data frame# --- # Uses Tsi_index Function 
Extractor <- function (from,to,type,s_s) {
  from.date <- strptime(from, format="%Y.%m.%d")
  to.date <- strptime(to, format="%Y.%m.%d")
  
  from_y <- format.Date(from.date,"%Y")
  from_m <- format.Date(from.date,"%m")
  from_d <- format.Date(from.date,"%d")
  to_y <- format.Date(to.date,"%Y")
  to_m <- format.Date(to.date,"%m")
  to_d <- format.Date(to.date,"%d")
  
  page_n <- as.numeric(round(difftime(to.date,from.date,units="days")/42,0))+1
  
  #Setting up a variable to be used for storage
  data_pages <- list()
  if(type != "stock"){
  # Loop in each page and get numbers
  for (p in 1:page_n) {
    data_pages[[p]] <- tsi_index(from_y,from_m,from_d,to_y,to_m,to_d,p)
  }# End Loop
  }else {
    for (p in 1:page_n) {
      data_pages[[p]] <- stock_data(from_y,from_m,from_d,to_y,to_m,to_d,s_s,p)
    } 
  }
  #Setting up a variable to be used for storage
  new_df <- data.frame()
  new_df <- data_pages[[1]]
  for (i in 2:length(data_pages)) {
    new_df <- rbind(new_df,data_pages[[i]])
  }# End Loop
  n.r <- dim(new_df)[[1]]
  new_df <- new_df[n.r:1,]
  return (na.omit(new_df))
}# End Function

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


#==================================================================================================================#
# Step 1: Modify "First_Junk" in tsi_index FUNCTION 
# Step 2: Provide From date (year,month,day) and To date (year,month,day) and number of pages to Extractor FUNCTION
# Step 3: Call calculate.Mu.Sigma to calculate Mu and sigma 
#==================================================================================================================#

library("gdata")
data <- Extractor("2015.01.01","2015.12.28","TASI")
data <- Extractor("2016.01.01","2016.02.22","stock",2010)
oil <- read.csv(url("http://www.quandl.com/api/v1/datasets/CHRIS/CME_BZ1.csv"), header = T)
banks <- read.xls("StockSymbols.xlsx", sheet = 1, header = TRUE)[,1:3]
petro <- read.xls("StockSymbols.xlsx", sheet = 2, header = TRUE)[,1:3]
head(petro)
head(banks)
b<- banks$St.Symbol
p<- petro$St.Symbol

stock_petro <- list()
for (i in 1:length(p)){
  stock_petro[[i]] <- Extractor(2015,01,01,2015,12,28,10,"stock",p[i])[-1,]
} 

stock_banks <- list()
for (i in 1:length(b)){
stock_banks[[i]] <- Extractor(2015,01,01,2015,12,28,10,"stock",b[i])[-1,]
} 
length(stock_petro)
#write.csv(stocks, "Etisalat.csv")

Sabic<-data[-1,]
Sab <- as.numeric(gsub(",","", Sabic$Close))
Petrochem <- as.numeric(gsub(",","", stock_petro[[2]]$Close))
RIBL <- as.numeric(gsub(",","", stock_banks[[1]]$Close))
tail(banks)
plot(1:length(Sab), Sab, type = "l")

pet.z <- scale(Petrochem)
ribl.z <- scale(RIBL)
cor(pet.z,ribl.z)
r <- range(pet.z,ribl.z)
plot(1:248,pet.z,ylim = r,type = "l", col= "red", cex = .8)
lines(1:248,ribl.z, col="blue")


mu <- calculate.Mu.Sigma(df)[[1]]
sigma <- calculate.Mu.Sigma(df)[[2]]


S <- G <- 0 
for (e in 1:1000){
  S[e] <- St(,mu,sigma,6934.06,mu,2)[[1]][5.5*60]
  G[e] <- St(6934.06,mu,sigma,6934.06,mu,2)[[2]]
}

etihad <- St(44.55,mu,sigma,30)
t.test(S)
hist(S, breaks= 50)
xn <- length(S)

# Google agianst 
yRange <- range(etihad)
plot(1:length(etihad),etihad, type = "l", ylim = yRange, col ="red", xlab = "# minutes", ylab = "Price", main = "etihad Simulation")
lines(1:xn,S, col = "blue")
legend(xn*0.65,yRange[2]*0.95,legend = c("Not Simulated","Simulated"), cex = 1, col = c("red","blue"), lty = 1)

