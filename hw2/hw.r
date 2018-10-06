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

for (ns in n)
{
  print(findMinM(ns,0.5))
}
