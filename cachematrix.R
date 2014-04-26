## Matrix inversion is usually a costly computation and their may be 
##some benefit to caching the inverse of a matrix rather than compute it repeatedly
##Computing the inverse of a square matrix can be done with the solve function 
##in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL     ## clear m
  ##set Function
  set <- function(y) {
    x <<- y       ##assign y value to x in the global environment     
    m <<- NULL   ## clear m in the global environment
  }
  ## get function
  get <- function() x
  ## setsolve Function
  setsolve <- function(solve) m <<- solve ## solve Function to inverse the Matrix
  ## getsolve , List of Functions
  getsolve <- function() m 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()  ## Call getsolve() function
  if(!is.null(m)) {  ## check If the inverse has already been calculated
    message("getting cached data")  
    return(m)      ## retrieve the inverse from the cache.
  }
  data <- x$get()      
  m <- solve(data, ...) ## call solve function
  x$setsolve(m)         ## set the inverse of the matrix
  m
}
