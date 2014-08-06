## Functions to cache information about a matrix and return its inverse quickly and efficiently. 

## meanCacheMatrix creates a list of functions related to a square, invertible matrix x. This caches the matrix so that
## the inverse can be easily calculated and stored. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns a matrix that is the inverse of x., taking as its input the cache created by
## makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <-  solve(data, ...)
  x$setinv(inv)
  inv
}
