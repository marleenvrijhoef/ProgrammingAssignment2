## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCachematrix below creates an object of class matrix. The function first sets the value of the matrix. Later it gets the value of the matrix en inverses the values. 

makeCachematrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m <<- inverse
      getInverse <- function() m
      list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## Write a short comment describing this function. 
## The cacheSolve function computes the inverse of the matrix above. If the inverse is already calculated, then the variabel retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

