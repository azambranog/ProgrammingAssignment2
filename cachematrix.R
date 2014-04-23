## We use the <<- operator to create two functions that 
## create an object that stores a matrux and cache's 
## its inverse.



## This function creates a matrix object which is a list
## that have functions for set and get the value of the 
## matrix and to get and set the value of its inverse

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) { 
    #change value on the matrix and make inverse NULL
    x <<- y
    inverse <<- NULL
  }
  get <- function() x  #get the value of the matrix
  setinverse <- function(inv) inverse <<- inv #get inverse
  getinverse <- function() inverse #set inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## this function calcualtes the inverse of a matrix
##created by the makeCacheMatrix function.
## If the inverse has been calculated and the matrix
## has not changed, then the calculation is skipped, 
## this is achieved because we set the inverse to null
## in x.set(), so, every time the matrix changes, then
## the inverse is set to NULL

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()#get the current inverse
  if(!is.null(inverse)) { 
    #if the inverse have been set, then get if from cahe
    message("getting cached data")
    return(inverse)
  }
  normal <- x$get()
  inverse <- solve(normal, ...)
  x$setinverse(inverse)
  inverse
}
