## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this function caches the inverse of the matrix passed to it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  # this function solves for the inverse of the matrix x if it does not already exist in the cache
  cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
