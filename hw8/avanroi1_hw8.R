library(ggplot2)
library("faraway")
require(faraway)
library("leaps")
require(MASS)
library("data.table", lib.loc="~/R/win-library/3.5")
library("MLmetrics", lib.loc="~/R/win-library/3.5")
library(reshape2)

#4b
set.seed(123)
numReleased=700
numSold=800000
numTrials=100000
numMovies = rpois(numTrials,numReleased)
counter = 0
oneYearTix=0
total=numeric()
while(counter<=numTrials)
{
  oneYearTix = rpois(numMovies[counter],numSold)
  total[counter] = sum(oneYearTix)
  counter = counter+1
}
print(mean(total))
print(numSold*numReleased)
print(var(total))
print(numSold^2 * numReleased + numSold*numReleased)


numTicks = rpois(numMovies,numSold)
print(sum(numTicks))
print(numReleased*numSold)
result=numMovies*numTicks
print(mean(numMovies)*mean(numTicks))
print(mean(result))
print(numReleased*numSold)
print(var(result))
print(numSold^2*numReleased+numReleased^2*numSold+numReleased*numSold)
