## Find inverse of a matrix.
## If it exists, use cached value. If not, find and cache it.

## makeCacheMatrix takes a matrix and attaches fxns for modifying it

makeCacheMatrix <- function(x = matrix()) {
  #makeCacheMatrix is a function object with the following
  # input matrix x
  # inverse matrix y
  # function set, which can set x = some matrix
  # function get, which returns x
  # function setInverse, which sets value of y
  # function getInverse, which gets value of y
  
  y <- NULL
  
  set <- function(a) {
    x <<- a
    y <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverted) {
    y <<- inverted
  }
  getInverse <- function() {
    y
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Calculates inverse of a matrix and caches it

cacheSolve <- function(x, ...) {
  #input matrix x
  #inverse matrix b
  
  #first, check if inverse has been calculated before
  b <- x$getInverse()
  if(!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  #if not, then get current matrix and find it's inverse
  data <- x$get()
  b <- solve(data, ...)
  x$setInverse(b)
  b
}