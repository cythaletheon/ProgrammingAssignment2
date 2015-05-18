## Coursera: R_Programming (rprog-014)
##    Programming Assignment 2
##
##    Student: Damon Grummet
##
##    File: cachematrix.R
##    
##    This program makes use of the lexical scoping rules in R to cache
##    the inverse of a given invertible matrix X
##
##    Usage:  
##   R> source('cachematrix.R')
##   R> aCached <- makeCacheMatrix(A)  ## where A is a square invertible matrix
##   R> aInv <- cacheSolve(aCache)
##   ## first run will compute the inverse using 'solve' and store into aCache.
##   ...
##   R> aInv2 <- cacheSolve(aCache)
##   ## subsequent runs will return the cached inverse rather than computing.


## makeCacheMatrix builds the caching structure for the given matrix

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mInverse <<- solve
  getInverse <- function() mInverse
  list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve expects to be given at the least a cache matrix object made with
##   makeCacheMatrix, and will return the inverse of the matrix stored within,
##   either by computation using 'solve' or from the cache if already filled.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  data <- x$get()
  mInverse <- solve(data, ...)
  x$setInverse(mInverse)
  mInverse
}
