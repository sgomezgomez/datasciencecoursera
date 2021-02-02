myfirstfunction <- function(){
  x <- rnorm(100)
  mean(x)
}

mysecondfunction <- function(x){
  x + rnorm(length(x))
}