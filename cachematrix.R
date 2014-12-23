######################################################################################
##
## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The 
## functions below implements such functionality.
## 
######################################################################################


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  getMatrix <- function() x
  setMatrix <- function(m) {
    x <<- NULL
    im <<- NULL
  }
  
  getInvertedMatrix <- function () im
  setInvertedMatrix <- function (invMatrix) im <<- invMatrix
    
  list(getMatrix = getMatrix, 
       setInvertedMatrix = setInvertedMatrix, 
       getInvertedMatrix = getInvertedMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getInvertedMatrix() 
  
  if (is.null(im)){
    im <-solve(x$getMatrix(),...)
    x$setInvertedMatrix(im)
  }
  
  im
}
