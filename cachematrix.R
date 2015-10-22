## There two functions are used to create a special matrix with cache for inverse matrix
## It helps to save time in elaboration because it saves the result of the "solve" function
## into a variable in the global environment and it recall that value for any further request of the
## inverse matrix after the first

## The makeCacheMatrix function creates a "special" version of the "x" matrix passed by parameter.
## This special matrix has methods to load and store the value of the inverse matrix using the cache

makeCacheMatrix <- function(m = matrix()) {
      s <- NULL
      set <- function(y){
            m <<- y
            s <<- NULL
      }
      
      get <- function() m
      setSolved <- function(solved) s <<- solved
      getSolved <- function() s
      
      list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}


## The cacheSolve method is used to retrieve the inverse of an "x" matrix passed by parameter using the cache.
## The "x" matrix must be created with the makeCacheMatrix function

cacheSolve <- function(x, ...) {
      s <- x$getSolved()
      if(!is.null(s)) {
            message("getting cached data")
            return(s) ##Here returns the matrix stored in the cache
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setSolve(s)
      s ##Here returns the solved matrix just calculated and stored in the cache
}

