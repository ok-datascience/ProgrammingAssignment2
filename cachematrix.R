## This module defines two functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache 
##                  its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned 
##             by makeCacheMatrix above. If the inverse has already been calculated 
##             (and the matrix has not changed), then cacheSolve should retrieve the
##             inverse from the cache.
######################################################################################


## Returns list represents matrix and  operations on it. Matrix is
## immutable in terms of creation, it means matrix value can be set only
## one time while list creation. Cache value can be changed arbitrary 
## number of times.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  getMatrix <- function() x
  getInvertedMatrix <- function () im
  setInvertedMatrix <- function (invMatrix) {
    im <<- invMatrix
  }
    
  list(getMatrix = getMatrix, 
              setInvertedMatrix = setInvertedMatrix, 
              getInvertedMatrix = getInvertedMatrix)
}


## return cached inverted matrix or in case of cache returns empty value,
## calculates inverted matrix and caches it value

cacheSolve <- function(x, ...) {
  im <- x$getInvertedMatrix() 
  
  if (is.null(im)){
    im <-solve(x$getMatrix())
    x$setInvertedMatrix(im)
  }
  
  im
}
