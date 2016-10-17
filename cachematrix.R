## Put comments here that give an overall description of what your
## functions do

#This function makeCacheMatrix creates a special "matrix", which is really a list containing
#a function to set the value of the matrix, get the value of the matrix, set the value of the inversion
#get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      #definites set function
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


##The following function calculates the inversion of the special "matrix" created with the above function. 
#However, it first checks to see if the inversion has already been calculated. If so, it gets the inversion from the cache and skips the computation. 
#Otherwise, it calculates the inversion of the data and sets the value of the mean in the cache via the setInversion function.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      # gets inverse from cache
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      # calculates inverse
      inv <- solve(data, ...)
      x$setInverse(inv)
}
