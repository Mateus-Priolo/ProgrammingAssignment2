## The two functions below are used 
## to cache the inverse of a matrix,
## in order to avoid computing it repeatedly.

## this function creates a list with a function that sets
## the value of the matrix, gets its value, sets the value
## of the inverse matrix and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(y){
    x<<- y
    i<<- NULL
  }
  get<- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This second function returns a matrix that is the inverse of 'x'. 
## At first, it checks if the inverse has already been computed.
## If so, it gets the outcome and skips the computation. 
## If not, it computes the inverse and sets the value in the cache
## with setinverse function.

cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
