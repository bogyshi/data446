library("leaps")
require(MASS)
library("data.table", lib.loc="~/R/win-library/3.5")
library("MLmetrics", lib.loc="~/R/win-library/3.5")
library(reshape2)

getMedianandMode= function(n,type)
{
  results = runif(n,0,1)
  data = data.frame(results)
  getMode = table(results)
  p<-ggplot(data=data, aes(y=results[,1])) +geom_bar(stat="identity")
  p
  #barplot(results,main="whatev",width=0.5)
  print(median(results))
  print(tail(getMode))
  #print(forMode[c(1:10),])
}

expoMedMode = function(n,rate)
{
  results = rexp(n,rate=rate)
  print(median(results))
  getmode = table(results)
  print(which.max(getmode))
}


#this is 1a
set.seed(123)
n=10000
getMedianandMode(n,"uniform")

#this is 2a
set.seed(123)
n=10000
expoMedMode(n,.7)
