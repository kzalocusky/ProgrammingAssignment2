## This set of functions creates an object that stores a 
##matrix and caches its mean

## creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInv <- function(solve) m <<- solve
      getInv <- function() m
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)   
}

## 

cacheSolve <- function(x, ...) {
      m <- x$getInv()
      #if the matrix inverse is already cached, retrieve that data
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      #otherwise, calculate the matrix inverse
      data <- x$get()
      m <- solve(data, ...)
      x$setInv(m)
      m
}
