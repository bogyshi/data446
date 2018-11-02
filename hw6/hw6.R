library(ggplot2)
library("faraway")
require(faraway)
library("leaps")
require(MASS)
library("data.table", lib.loc="~/R/win-library/3.5")
library("MLmetrics", lib.loc="~/R/win-library/3.5")
library(reshape2)

binUnif= function(n,a,b,intv,mfactor)
{
  message = paste("From",a)
  message = paste(message,"to")
  message = paste(message,b)
  print(message)
  resultsog = runif(n,a,b)
  results = floor(resultsog*mfactor)
  data = data.frame(results)
  vals = seq(a,b,intv)
  pdf(paste(b-1,"graph.pdf",sep="")) 
  p<-ggplot(data=data, aes(x=factor(data[,1]))) +geom_bar(stat="count") + scale_x_discrete(labels=vals)
  print(p)
  dev.off()
  #barplot(results,main="whatev",width=0.5)
  print(median(resultsog))
  getmode = table(resultsog)
  print(which.max(getmode))
  #print(forMode[c(1:10),])
}
expoMedMode = function(n,rate)
{
  resultsog = rexp(n,rate=rate)
  results = floor(resultsog*10)
  data = data.frame(results)
  data = data.frame(data[data[,1]<25,])
  vals = seq(0,2.5,.1)
  p<-ggplot(data=data, aes(x=factor(data[,1]))) +geom_bar(stat="count") + scale_x_discrete(labels=vals)
  print(p)
  #barplot(results,main="whatev",width=0.5)
  print(median(resultsog))
  getmode = table(resultsog)
  print(which.max(getmode))
}
chiqTest = function(truMatrix,expectedMatrix)
{
  sum =0
  rcounter = 1
  ccounter = 1
  sum = 0
  while(rcounter <= 2)
  {
    ccounter = 1
    while(ccounter<=2)
    {
      sum = sum + (((truMatrix[rcounter,ccounter]-expectedMatrix[rcounter,ccounter])**2)/expectedMatrix[rcounter,ccounter])
      ccounter= ccounter + 1
    }
    rcounter = rcounter +1
  }
  return(sum)
}

prob6a = function(n)
{
  x = rnorm(n,0,1)
  y = rnorm(n,0,1)
  xmy = x-y
  xpy = x+y
  return(cov(xmy,xpy))
}

gsquareTest = function(truMatrix,expectedMatrix)
{
  sum =0
  rcounter = 1
  ccounter = 1
  sum = 0
  while(rcounter <= 2)
  {
    ccounter = 1
    while(ccounter<=2)
    {
      sum = sum + (truMatrix[rcounter,ccounter]*log(truMatrix[rcounter,ccounter]/expectedMatrix[rcounter,ccounter]))
      ccounter= ccounter + 1
    }
    rcounter = rcounter +1
  }
  return(2*sum)
}

#problem 6a
print(prob6a(100000))

y = matrix(c(0.018, 0.035 , 0.031 ,0.008 , 0.018 ,
                     0.002,0.112 ,0.064,0.032,0.069,
                     0.001,0.066 , 0.094 ,0.032, 0.084,
                     0.001 , 0.018, 0.019, 0.010, 0.051,
                     0.001, 0.029 , 0.032,0.043,0.130), nrow=5 , byrow=TRUE)
colnames ( y ) = c ( 'farm' , "operatives",'craftsen','sales','professional' )
rownames ( y ) = colnames ( y )

sum( y )
#8a
sum(y[1,])
#[1] 0.11
sum(y[2,])
#[1] 0.279
sum(y[3,])
#[1] 0.277
sum(y[4,])
#[1] 0.099
sum(y[5,])
#[1] 0.235

#8b
sum(y[,1])
#[1] 0.023
sum(y[,2])
#[1] 0.26
sum(y[,3])
#[1] 0.24
sum(y[,4])
#[1] 0.125
sum(y[,5])
#[1] 0.352

#8c
result = y[1,]/sum(y[1,])
sum(result)
print(result)

#8d
resultd = y[,1]/sum(y[,1])
print(resultd)

#9a
#placebo = 28+656=684
#aspirin = 18+658=676
#yes = 28+18 = 46
#no = 656+658 = 1314
placebo = 684
aspirin = 676
yes = 46
no = 1314
grandtotal = yes+no
table9 = matrix(c(28,656,18,658), nrow=2 , byrow=TRUE)
colnames(table9) = c('yes','no')
rownames(table9) = c('Placebo','aspirin')
table9
#under no interaction, we have a simple set of multiplications
p1a=sum(table9[1,])/grandtotal
p2a=sum(table9[2,])/grandtotal
pa1=sum(table9[,1])/grandtotal
pa2=sum(table9[,2])/grandtotal
icell11=p1a*pa1
icell12=p1a*pa2
icell21=p2a*pa1
icell22=p2a*pa2
# since each cell is a bernoulli RV, our expected value is simply the probability times the grand total
iptable = matrix(c(icell11,icell12,icell21,icell22),nrow=2,byrow=TRUE)
ipttable = iptable*grandtotal
print(dpttable)
#under interaction we have
dcell11= table9[1,1]/grandtotal
dcell12= table9[1,2]/grandtotal
dcell21= table9[2,1]/grandtotal
dcell22= table9[2,2]/grandtotal
dptable = matrix(c(dcell11,dcell12,dcell21,dcell22),nrow=2,byrow=TRUE)
dpttable = dptable*grandtotal
print(dpttable)
##chisq.test(x=iptable,y=dptable)

test1 = (chiqTest(table9,ipttable))
test2 = (gsquareTest(table9,ipttable))
print(test1)
print(test2)
print(1-pchisq(test1,1))
print(1-pchisq(test2,1))

#conclusion
#btoh values are similar indicating a low confidence that the data is in fact abnormal to the null hypothesis, at least at the .05 signifigance level