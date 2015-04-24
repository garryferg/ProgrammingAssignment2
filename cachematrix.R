## The following functions work together to provide a special matrix type
## that supports caching of its inverse (in order to avoid repeating expensive
## computation).
##
## Usage example:
## m <- makeCacheMatrix(matrix(c(0.5, 0, 0, 1), ncol=2, nrow=2))
## cacheSolve(m)  # calculates and returns the inverse of m
## cacheSolve(m)  # returns the previously cached inverse of m
## cacheSolve(m)  # returns the previously cached inverse of m
## m$set(matrix(c(1, 0, 0, 0.5), ncol=2, nrow=2)) #updates the matrix value
## cacheSolve(m)  # calculates and returns the new inverse of m
## cacheSolve(m)  # returns the new cached inverse of m


## 'makeCacheMatrix' creates a special type of matrix that supports storing
## a cached copy of its inverse.
## The function accepts an optional parameter, x, used to set the initial
## value of the stored matrix.
## The returned list has four named items, all of which are functions:
##   get and set(newMatrix) - get and set the value of the stored matrix.
##   getInverse and setInverse(inverse) - get and set the value of the stored
##     inverse - (used by "cacheSolve", not intended to be called directly)

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve' returns the inverse of a special matrix created with
## 'makeCacheMatrix'. If the special matrix already has a cached inverse that
## will be returned, otherwise the inverse will be calculated, stored in the
## special matrix, then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
