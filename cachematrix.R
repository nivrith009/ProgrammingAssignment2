
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # This is a private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # This is a private function used to get the cached inverse of x



## Create a cacheMatrix object for an inverted matrix.
## functions do

makeCacheMatrix <- function(x = matrix()) {
	  cachedInverse <- NULL
	  set <- function(y) {
	    x <<- y
	    cachedInverse <<- NULL
	  }
	  get <- function() x
	  setInverse <- function(inverse) cachedInverse <<- inverse
	  getInverse <- function() cachedInverse
	  list(set = set, get = get,
	       setInverse = setInverse,
	       getInverse = getInverse)
	}

 ## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	  
	  invFunc <- x$getInverse()
	  if(!is.null(invFunc)) {
	    message("getting cached data")
	    return(invFunc)
	  }
	  data <- x$get()
	  invFunc <- solve(data, ...)
	  x$setInverse(invFunc)
	  invFunc
	}
