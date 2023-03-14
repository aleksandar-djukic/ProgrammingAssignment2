#functions are used to create and cache the inverse of a matrix in R.

## 
#The makeCacheMatrix() function creates a new matrix object and returns a list 
#of four functions: set(), get(), setinv(), and getinv(). 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
      x<<-y
      inv<<-NULL
  }
  get <- function()x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
      inver <- ginv(x)
      inver%*%x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#if statement checks if inverse is NULL, and then returns inverse value
# The function return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) #function gets the cached data
{
  inv <- x$getinv()
  if (!is.null(inv)){
      message("getting chached data!")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv 
}
