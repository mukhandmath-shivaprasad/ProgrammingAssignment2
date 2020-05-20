## Assignmrnt 2
## This function creates a special "matrix" object that can cache its inverse. 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        
        set <- function(y) {
        
        x <<- y
        matinv <<- NULL
  }
  
  get <- function() x
  
  setInver <- function(cache) matinv <<- cache
  
  getInver <- function() matinv
  
  list(set = set, get = get,setInver = setInver,getInver = getInver)
}


## Write a short comment describing this function
# Assignment 2 - cacheSolve
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matinv <- x$getInver()
        
        if(!is.null(matinv)) {
          
          message("getting cached data")
          
          return(matinv)
        }
        
        data <- x$get()
        
        matinv <- solve(data, ...)
        
        x$setInver(matinv)
        
        matinv
}
