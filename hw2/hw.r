#HW2 problem number 5
?pbinom
pbinom(0,10,.5)
n=c(10,100,1000,10000)
findMinM = function(n,p){
  m=(n/2) #minus 1 for first step of loop
  lpages=floor(m)
  rpages=ceiling(m)
  lselected=n/2 # always start with both pages getting the same number of pages drawn from them
  sum = dbinom(lpages,n,p)
  diff=0
  while (TRUE) {
    
    if(sum>=0.95)
    {
      return(m)
    }
    else
    {
      m= m+1
      lpages=floor(m)
      rpages=ceiling(m)
      lselected=floor(n/2)
      rselected=ceiling(n/2)
      sum = dbinom(lselected,n,p)
      lselected=lselected-1
      diff=1
    }
    
    while((rpages>=n-lselected) & lselected>=0)
    {
      sum= sum+(dbinom(lselected,n,p)*2)     #done because symmetric about the axis
      diff= diff+1
      lselected=lselected-1
    }
    #print(sum)
  }
  
}

findMinM2 = function(n,p){
  m=(n/2) #minus 1 for first step of loop
  lselected=m
  rselected=m
  numTries=10000
  while (TRUE) {
    numCorrect=0
    results = rbinom(numTries,n,0.5)
    l = ((m-results)>=0)
    r = ((m - (n-results))>=0)
    numCorrectvec=l&r
    numCorrect=length(numCorrectvec[numCorrectvec=="TRUE"])
    #print(numCorrect)
    sum = numCorrect/numTries
    if(sum>=0.95)
    {
      return(m)
    }
    else
    {
      m= m+1
    }
  }
  
}

for (ns in n)
{
  print(findMinM2(ns,0.5))
}

for (ns in n)
{
  print(findMinM(ns,0.5))
}
