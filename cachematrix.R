## Programming Assignment 2: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## For this assignment, assume that the matrix supplied is always invertible.
##Solution

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  setmatrix <- function(matrix ) 
  {
    x <<- matrix
    inv <<- NULL
  }
  
  ## Method to get the matrix
  getmatrix <- function() 
  {
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return a list of the methods
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

##Compute the inverses of the special matrix returned by "makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" will retrieve the inverse from the cache.

##Solution

## The following function returns the inverse of the special matrix. It first checks if
## the inverse has already been calculated, if yes, it gets the result from the cache. If not, it 
## calculates the inverse, sets the value in the cache via
## setinverse function.


cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$getmatrix()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
