## Put comments here that give an overall description of what your
## functions do

## Function constracts matrix with cash inverse functionality

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Client function to construct matrix and set inverse value

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  library(MASS)
  data <- x$get()
  inv <- ginv(data)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
