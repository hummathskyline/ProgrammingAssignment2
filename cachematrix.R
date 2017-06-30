## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  theMatrixInverse <- NULL
  
  ## print("Entering makeCachmatrix")
  ## print(x)
  
  ## setter function
  set <- function(y) {
    x <<- y
    theMatrixInverse <<- NULL
  }
  
  ## getter function
  get <- function() x
  
  setInverse <- function(matrixInverse) theMatrixInverse <<- matrixInverse
  getInverse <- function() theMatrixInverse
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {


  theMatrixInverse <- x$getInverse()

  ## return the cached inverse if it's cached
  if(!is.null(theMatrixInverse)) {
      message("getting cached data")
      return(theMatrixInverse)
  }
  
  ## otherwise compute the inverse and cache it
  theMatrix <- x$get()
  theMatrixInverse <- solve(theMatrix, ...)
  x$setInverse(theMatrixInverse)
  theMatrixInverse
  
  
  
  }

m <- matrix( c(1,-1,1,2),nrow = 2, ncol=2)
##m
##mInv <- solve(m)
##mInv

myMatrix <- makeCacheMatrix(m)
myMyMatrix <- myMatrix$get()
myMyMatrix

cacheSolve(myMatrix)
