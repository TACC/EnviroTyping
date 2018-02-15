setwd("/home1/04734/dhbrand/training")

myProc <- function(size=10000000){
  #Load a large vector
  vec <- rnorm(size)
  
  #Now sleep on it
  Sys.sleep(2)
  
  #Now sum the vec values
  return(sum(vec))
}
ptm <- proc.time()
myProc(100000000)

proc.time() - ptm

Sys.getpid()

system('top -b -n 1 -u $USER', intern=TRUE)

ptm <- proc.time()
result <- c()
for(i in 1:10){
  result <- c(result,myProc())
}
proc.time() - ptm
