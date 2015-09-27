## This file contains functions for calculation of a matrix inverse
## and caching the output for future use to avoid repeated expensive 
## caluclations. 

## there are two functions that were created to do this - makeCacheMatrix
## & cacheSolve

## makeCacheMatrix creates a special "vector" which is really a list.
## It stores the matrix whose inverse is to be calculated

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix in the special "vector" 
## created with the above function. It first checks if the inverse has been
## calculated before (this avoids) repeated inverse calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  ## solve is the R function for calculating inverse of a matrix
  
  inv <- solve(data, ...)     
  x$setinv(inv)
  inv
}
