## Assignment 2 for R-Programming course in DS track
## Pair of functions that cache the inverse of a matrix

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the matrix and clear cache
  # <<- means alter in parent env
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get matrix stored in x
  get <- function() x
  # save cache
  setinv <- function(inv) m <<- inv
  # get cache
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix() above

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  # same matrix and inv already claculated
  # retrieve inv from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # calculate inv if cache empty
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
