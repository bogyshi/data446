set.seed(123)
#this is 2a
n=1000000
results = runif(n,0,1)
counter = 1
areas=numeric(0)
while(counter<n)
{
  areas[counter] = results[counter] * results[counter] * pi
  counter= counter + 1
}
print(mean(areas))
print(var(areas))

#this is 3a
set.seed(123)
n= 10000000
results = runif(n,0,1)
counter = 1
xs = numeric(0)
ys = numeric(0)
while(counter <= n)
{
  if(results[counter]>=0.5)
  {
    ys[counter]=results[counter]
    xs[counter] = 1-ys[counter]
  }
  else
  {
    xs[counter] = results[counter]
    ys[counter] = 1 -xs[counter]
  }
  counter = counter + 1
}
rs = xs/ys
print(mean(rs))
print(mean(1/rs))
og = runif(n,0,1)
plot(x=c(1:n),y=rs)
plot(x=c(1:n),y=og,col='red')


#4c
set.seed(123)
n=10
runplenty=10000
og = runif(n,0,1)
counter=1
result= numeric(0)
while(counter<=runplenty)
{
  og = runif(n,0,1)
  result[counter]=max(og)
  counter= counter+1
}
print(mean(result))
print(n/(n+1))


#5b
set.seed(123)
n= 1000000
resultsx = rnorm(n,0,1)
resultsy = rnorm(n,1,5)
trueResults = rnorm(n,-1,sqrt(26))
counter = 1
numCorr=0
numCorr2=0
results=numeric(0) 
while(counter<=n)
{
  if(resultsx[counter]<resultsy[counter])
  {
    numCorr = numCorr + 1
  }
  if(trueResults[counter]<0)
  {
    numCorr2 = numCorr2 + 1
  }
  counter= counter+1
}
print(numCorr/n)
print(numCorr2/n)

#6b
set.seed(123)
n=100
numTrials=10000
counter=1
diffRes = numeric(0)
while(counter<numTrials)
{
  resultsx = rnorm(n,69.1,2.9)
  resultsy = rnorm(n,63.7,2.7)
  diffRes[counter] = mean(resultsx)-mean(resultsy)
  counter = counter + 1
}
resultCalc = rnorm(n,5.4,sqrt(15.7/n))
print(mean(diffRes))
print(mean(resultCalc))
print(var(diffRes))
print(var(resultCalc))
print(1-pnorm(0,5.4,sqrt(15.7/n)))
#7b,c
set.seed(123)
calcwithy = function(y,pindex)
{
  set.seed(123)
  n=5
  numdifps = 11
  counter = 1
  denom=0
  numer=0
  probs=c(0.0,0.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)
  while(counter<=numdifps)
  {
    val = dbinom(y,n,probs[counter]) # not simulatin
    #print(probs[counter])
    if(counter == pindex)
    {
      numer = val
    }
    denom = denom + val
    counter = counter + 1
  }
  print(numer)
  return(numer/denom)
}

for (yval in (0:5))
{
  results=numeric(0)
  for (num in (1:11))
  {
    results[num] = calcwithy(yval,num)
  }
  title = paste("y=",yval,sep="")
  xlabel = "0.0,0.1,0.2,...0.9,1.0"
  pdf(paste(title,"graph.pdf",sep="")) 
  plot(x=c(1:11),y=results,xlab=xlabel,ylab="probabilites",pch=16,main = title)
  dev.off()
}
#8b
set.seed(123)
n=10000
unifVals = runif(n,0,1)
invert = log(unifVals/(1-unifVals))
res = (invert<3 & invert>2)
print(sum(res == TRUE))
print(sum(res==TRUE)/n)