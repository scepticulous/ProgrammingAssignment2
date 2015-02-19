## This module implements a cached version of the matrix inverxe operation
## by storing the result in the current environment

# create a list of a matrix and some operations that will serve
# as some kind of a smart-matrix that supports caching the inverse operation
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(arg) inverse <<- arg
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# caculate the inverse of a matrix.
# this function tries to retrieve a cached value
# and only calculates the inverse if that value is not present yet

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix_ <- x$get()
  inverse <- solve(matrix_, ...)
  x$set_inverse(inverse)
  inverse
}
