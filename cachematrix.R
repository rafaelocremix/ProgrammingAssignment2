## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# returns a matrix "object" that can store previous
# calculations of its inverse. Tested with diag()

makeCacheMatrix <- function(x = matrix()) {
  # attributes
  x <- x
  x.inv <<- NULL
  
  # interface methods
  getmatrix <- function() {x} # returns the matrix
  getinverse <- function() {x.inv} #returns the inverse
  setinverse <- function(inv) {x.inv <<- inv} # set the inverse
  setmatrix <- function(mtx) {
    # set the matrix and nullifies previous inverses
    x <<- mtx
    x.inv <<- NULL
  }
  return(list(getm = getmatrix, getinv = getinverse,
              setm = setmatrix, setinv = setinverse))
}


## Write a short comment describing this function
# I have choosen to use the "methods" instead of the intermediary values
# to create a shorter code, I believe it is still readable.
# I also check for the inverse before any calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (!is.null(x$getinv())) {
    # returns the inverse if any
    return(x$getinv()) 
    
  } else {
    
    # calculates the inverse if none
    x$setinv(solve(x$getm(), ...))
    return(x$getinv())
  }
}

## test settings
x <- makeCacheMatrix(matrix(c(1:4), nrow = 2, ncol = 2))
system.time(cacheSolve(x)) # calculation runtime
system.time(cacheSolve(x)) # cached runtime

x <- makeCacheMatrix(diag(x = 1, nrow = 100, ncol = 100))
system.time(cacheSolve(x)) # calculation runtime
system.time(cacheSolve(x)) # cached runtime

x <- makeCacheMatrix(diag(x = 1, nrow = 1000, ncol = 1000))
system.time(cacheSolve(x)) # calculation runtime
system.time(cacheSolve(x)) # cached runtime

#rm(list = ls())
