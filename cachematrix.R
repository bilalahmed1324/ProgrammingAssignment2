## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL 
  set <- function (y = matrix() ) {
    
      x<<- y
      inv<<-NULL
    
          } 
  get <- function() x
  
  setinverse <- function( inverse=matrix()) inv<<- inverse
  getinverse <- function () inv 
  
  
  list ( set = set, get =  get, setinverse= setinverse , getinverse=getinverse)
  
  
}


##  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    y <- x$getinverse()
    xorig <- x$get()
    if (!is.null(y) && is.matrix(xorig) && 
              is.matrix(x$get()) && dim(xorig) == dim(x$get()) 
              && all(xorig == x$get()) ) {
        message ("getting cached matrix")
        return(y)
    }
    inv<-solve(xorig)
    x$setinverse(inv)
    inv
  
}
