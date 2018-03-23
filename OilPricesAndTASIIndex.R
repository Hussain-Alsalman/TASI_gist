
layout(1:1)
plot(data$close, type = "l")
install.packages("Quandl")
library("Quandl")
oil <- Quandl("OPEC/ORB")



plot(data$close, type = "l")
oil_sub<-oil[which(oil$Date > as.Date("2017-01-01") & oil$Date < as.Date("2018-03-19")),]
oil.scaled <- scale(oil_sub$Value)
tasi.scaled <- scale(data$close)
length(oil.scaled)
length(tasi.scaled)
truncted.tasi.sub <- tasi.scaled[1:346]
plot(1:length(tasi.scaled),tasi.scaled, type = "l")
lines(1:(length(oil.scaled)-5),oil.scaled[306:1], col= "red")