set.seed(123)
#this is 3a
n= 1000
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
og = runif(n,0,1)
plot(x=c(1:n),y=rs)
plot(x=c(1:n),y=og,col='red')
#4c
n=10
runplenty=1000
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