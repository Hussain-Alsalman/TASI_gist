
#FUNCTION: Parsing URL call
parseURL <- function(p,fromDate, toDate, comSymbol = NULL , type, adjustment ="no") {
  if (type == "company") {
    firstJunk <-"https://www.tadawul.com.sa/wps/portal/tadawul/market-participants/issuers/issuers-directory/company-details/!ut/p/z1/pdDJDoIwFAXQr2HdSwFFdwgKlSHBAbUbUzUqCdMCNfr1FnVj4hDj2zU5973cEk7mhBfimO5EnZaFyOR7wVvLyAtsDyb13f5Eg9VyQnsUMQqAzG6AUttUOzoCBG1VAhcsDnUNsUb4T3mXRW1YseUlg0RSk_6Xh_4tPyR8l5Wre9V9XVddBQpqsRGnQ6aA8HWZV6I4j8_5qpRIhdoc5c974VFH7u37vmMbFD3jAT79yzN4UfwjaJrdAN6MBVLl0-n8EmzHLGVXY0T5Tw!!/p0/IZ7_NHLCH082KGET30A6DMCRNI2086=CZ6_NHLCH082KGET30A6DMCRNI2000=NJhistoricalPerformance=/?draw=2&columns%5B0%5D%5Bdata%5D=transactionDate&columns%5B0%5D%5Bname%5D=&columns%5B0%5D%5Bsearchable%5D=true&columns%5B0%5D%5Borderable%5D=true&columns%5B0%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B0%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B1%5D%5Bdata%5D=todaysOpen&columns%5B1%5D%5Bname%5D=&columns%5B1%5D%5Bsearchable%5D=true&columns%5B1%5D%5Borderable%5D=true&columns%5B1%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B1%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B2%5D%5Bdata%5D=highPrice&columns%5B2%5D%5Bname%5D=&columns%5B2%5D%5Bsearchable%5D=true&columns%5B2%5D%5Borderable%5D=true&columns%5B2%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B2%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B3%5D%5Bdata%5D=lowPrice&columns%5B3%5D%5Bname%5D=&columns%5B3%5D%5Bsearchable%5D=true&columns%5B3%5D%5Borderable%5D=true&columns%5B3%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B3%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B4%5D%5Bdata%5D=previousClosePrice&columns%5B4%5D%5Bname%5D=&columns%5B4%5D%5Bsearchable%5D=true&columns%5B4%5D%5Borderable%5D=true&columns%5B4%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B4%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B5%5D%5Bdata%5D=change&columns%5B5%5D%5Bname%5D=&columns%5B5%5D%5Bsearchable%5D=true&columns%5B5%5D%5Borderable%5D=true&columns%5B5%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B5%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B6%5D%5Bdata%5D=changePercent&columns%5B6%5D%5Bname%5D=&columns%5B6%5D%5Bsearchable%5D=true&columns%5B6%5D%5Borderable%5D=true&columns%5B6%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B6%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B7%5D%5Bdata%5D=volumeTraded&columns%5B7%5D%5Bname%5D=&columns%5B7%5D%5Bsearchable%5D=true&columns%5B7%5D%5Borderable%5D=true&columns%5B7%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B7%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B8%5D%5Bdata%5D=turnOver&columns%5B8%5D%5Bname%5D=&columns%5B8%5D%5Bsearchable%5D=true&columns%5B8%5D%5Borderable%5D=true&columns%5B8%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B8%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B9%5D%5Bdata%5D=noOfTrades&columns%5B9%5D%5Bname%5D=&columns%5B9%5D%5Bsearchable%5D=true&columns%5B9%5D%5Borderable%5D=true&columns%5B9%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B9%5D%5Bsearch%5D%5Bregex%5D=false&start="
    return (
      paste(
        firstJunk,p,"&length=30&search%5Bvalue%5D=&search%5Bregex%5D=false&isNonAdjusted=0&startDate=", fromDate, "&toDate=" ,toDate,"&symbol=",comSymbol,"&_=1463986258725",sep = ""
      )
    )
  }
  if (type == "index") {
    firstJunk <- "https://www.tadawul.com.sa/wps/portal/tadawul/markets/equities/indices/today/!ut/p/z1/rZFNTwIxEIZ_C4cepbOwgnprTFhrViIRcO1lU7pVavqxaQuL_97CzUSXGJ3bZJ4nM28GM1xhZvlevfGonOU69S9sUo9Gt1fZdQ4llNMMyKQAunjIx8UM8HMfAEWG2a_8gs6nQBbkbj1bL5M__psP-Xmf9SLzrB84RfwKfJOhFzgeeQLghyKA78_lSI_axtjeIEDQdd1QbcxQOIPgYLQNCFrvGgQNjzx-tBKBcDZKGxF4GdzOC3khnNZSHP8eMIt8Q20jD7h6lP7VecOtkP-9RGy5j3XkQdVi530i65CGzuNqSZ4obs1qVYGi75d6X5LB4BP_kbdL/p0/IZ7_NHLCH082KGN530A68FC4AN2OM2=CZ6_22C81940L0L710A6G0IQM43GF0=N/?draw=25&columns%5B0%5D%5Bdata%5D=date&columns%5B0%5D%5Bname%5D=&columns%5B0%5D%5Bsearchable%5D=true&columns%5B0%5D%5Borderable%5D=true&columns%5B0%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B0%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B1%5D%5Bdata%5D=open&columns%5B1%5D%5Bname%5D=&columns%5B1%5D%5Bsearchable%5D=true&columns%5B1%5D%5Borderable%5D=true&columns%5B1%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B1%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B2%5D%5Bdata%5D=high&columns%5B2%5D%5Bname%5D=&columns%5B2%5D%5Bsearchable%5D=true&columns%5B2%5D%5Borderable%5D=true&columns%5B2%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B2%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B3%5D%5Bdata%5D=low&columns%5B3%5D%5Bname%5D=&columns%5B3%5D%5Bsearchable%5D=true&columns%5B3%5D%5Borderable%5D=true&columns%5B3%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B3%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B4%5D%5Bdata%5D=close&columns%5B4%5D%5Bname%5D=&columns%5B4%5D%5Bsearchable%5D=true&columns%5B4%5D%5Borderable%5D=true&columns%5B4%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B4%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B5%5D%5Bdata%5D=totalVolume&columns%5B5%5D%5Bname%5D=&columns%5B5%5D%5Bsearchable%5D=true&columns%5B5%5D%5Borderable%5D=true&columns%5B5%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B5%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B6%5D%5Bdata%5D=totalTurnover&columns%5B6%5D%5Bname%5D=&columns%5B6%5D%5Bsearchable%5D=true&columns%5B6%5D%5Borderable%5D=true&columns%5B6%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B6%5D%5Bsearch%5D%5Bregex%5D=false&columns%5B7%5D%5Bdata%5D=noOfTrades&columns%5B7%5D%5Bname%5D=&columns%5B7%5D%5Bsearchable%5D=true&columns%5B7%5D%5Borderable%5D=true&columns%5B7%5D%5Bsearch%5D%5Bvalue%5D=&columns%5B7%5D%5Bsearch%5D%5Bregex%5D=false&order%5B0%5D%5Bcolumn%5D=0&order%5B0%5D%5Bdir%5D=desc&start="
    fromDate <- strptime(fromDate, format = "%Y-%m-%d") ; toDate <- strptime(toDate, format = "%Y-%m-%d")
    
    fromY <- format.Date(fromDate, "%Y") ; toY <- format.Date(toDate, "%Y")
    fromM <- format.Date(fromDate, "%m") ; toM <- format.Date(toDate, "%m")
    fromD <- format.Date(fromDate, "%d") ; toD <- format.Date(toDate, "%d")
    
    return (
      paste(
        firstJunk,p,"&length=10&search%5Bvalue%5D=&search%5Bregex%5D=false&sourceCallerId=datePicker&dateParameter=", fromY,"%2F",fromM,"%2F",fromD,"+-+",toY,"%2F",toM,"%2F",toD,"&typeOfCall=", ifelse(adjustment =="no",  "adjustedType","nonAdjustedType&old_tasi_current_sector=TASI"),sep = ""
      )
    )
  }
  
}

#install.packages("rjson")  # only if not installed already
library("rjson")
#Private FUNCTION: to get the index
privateGetIndex <-function(startDate, endDate, adjustPeriod){
  nRecords <-  fromJSON(file= parseURL(0, startDate, endDate, type = "index", adjustment = adjustPeriod))$recordsFiltered
  ifelse(nRecords <= 10, nPages <- 1, nPages <- ceiling(nRecords/10)) 
  
  fullData <- data.frame(stringsAsFactors = FALSE)
  for (i in 0:nPages ) {
    jsonData <- fromJSON(file = parseURL((i*10), startDate, endDate, type = "index", adjustment = adjustPeriod))
    p.table <- t(sapply(jsonData$data, function(x) unlist(x)))
    fullData <- rbind(fullData, as.data.frame(p.table, stringsAsFactors = FALSE))
  }
  #Formating the table
  fullData$date <- strptime(fullData$date, format = "%Y/%m/%d")
  fullData$high <- as.numeric(gsub(",","", fullData$high))
  fullData$open <- as.numeric(gsub(",","", fullData$open))
  fullData$low <- as.numeric(gsub(",","", fullData$low) )
  fullData$close <- as.numeric(gsub(",","", fullData$close))
  fullData$noOfTrades <- as.numeric(gsub(",","", fullData$noOfTrades))
  fullData$totalVolume <- as.numeric(gsub(",","", fullData$totalVolume))
  return (fullData[nRecords:1,])
}

#FUNCTION: get all records of TASI for a specified period. 
getIndexRecords <- function(fromDate, toDate){
  
  cutoffDate <- strptime("2008-04-02"  , format = "%Y-%m-%d")
  stY<-strptime(fromDate, format = "%Y-%m-%d")
  endY<-strptime(toDate, format = "%Y-%m-%d")
  
  if(stY<=cutoffDate & endY <= cutoffDate) {
    period <- "AllBeforeRestructure"
    return(privateGetIndex(startDate = fromDate, endDate = toDate, adjustPeriod = "yes"))
  }
  if((stY<=cutoffDate & endY >cutoffDate)){
    period <- "Combination"
    oldPart<- privateGetIndex(startDate = fromDate, endDate = "2008-04-02", adjustPeriod = "yes")
    newPart<- privateGetIndex(startDate = "2008-04-05", endDate = toDate, adjustPeriod = "no")
    oldPart$close * 0.9801111;
    return(rbind(oldPart,newPart)); } 
  if((stY >cutoffDate)){
    period <- "NewIndex"
    return(privateGetIndex(startDate = fromDate, endDate = toDate, adjustPeriod = "no"))
  }
}

#FUNCTION: get all records of a specified company for a specified period. 
getCompanyRecords <- function (startDate, endDate, companySymbol){
  nRecords <-  fromJSON(file= parseURL(0, startDate, endDate, companySymbol, type = "company"))$recordsFiltered
  ifelse(nRecords <= 30, nPages <- 1, nPages <- ceiling(nRecords/30))
  
  fullData <- data.frame(stringsAsFactors = FALSE)
  for (i in 0:nPages ) {
    jsonData <- fromJSON(file = parseURL((i*30), startDate, endDate, companySymbol, type = "company"))
    p.table <- t(sapply(jsonData$data, function(x) unlist(x)))
    fullData <- rbind(fullData, as.data.frame(p.table, stringsAsFactors = FALSE))
  }
  
  #Formating the table
  fullData$transactionDate <- strptime(fullData$transactionDate, format = "%b %e, %Y")
  fullData$previousClosePrice <- as.numeric(fullData$previousClosePrice)
  fullData$todaysOpen <- as.numeric(fullData$todaysOpen)
  fullData$highPrice <- as.numeric(fullData$highPrice)
  fullData$lowPrice <- as.numeric(fullData$lowPrice)
  fullData$volumeTraded <- as.numeric(fullData$volumeTraded)
  fullData$turnOver <- as.numeric(fullData$turnOver)
  fullData$noOfTrades <- as.numeric(fullData$noOfTrades)
  fullData$lastTradePrice <- as.numeric(fullData$lastTradePrice)
  fullData$change <- as.numeric(fullData$change)
  fullData$changePercent <- as.numeric(fullData$changePercent)
  
  return (fullData[nRecords:1,])
}


#===========================================================================================================================
#Example, Getting The Index Datagi
#=> Getting the Index Data
IndexData <- getIndexRecords("2017-01-01","2018-3-19")
#=> Getting a Company Data (SABIC)
CompanyData <- getCompanyRecords("2017.01.01","2018.3.19",2010)



#cor(truncted.oil.sub[340:1],tasi.scaled)
# mu <- calculate.Mu.Sigma(df)[[1]]
# sigma <- calculate.Mu.Sigma(df)[[2]]
# 
# 
# S <- G <- 0 
# for (e in 1:1000){
#   S[e] <- St(,mu,sigma,6934.06,mu,2)[[1]][5.5*60]
#   G[e] <- St(6934.06,mu,sigma,6934.06,mu,2)[[2]]
# }
# 
# etihad <- St(44.55,mu,sigma,30)
# t.test(S)
# hist(S, breaks= 50)
# xn <- length(S)
# 
# # Google agianst 
# yRange <- range(etihad)
# plot(1:length(etihad),etihad, type = "l", ylim = yRange, col ="red", xlab = "# minutes", ylab = "Price", main = "etihad Simulation")
# lines(1:xn,S, col = "blue")
# legend(xn*0.65,yRange[2]*0.95,legend = c("Not Simulated","Simulated"), cex = 1, col = c("red","blue"), lty = 1)

