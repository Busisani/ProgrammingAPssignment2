## makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse.

## The main purpose is to store a matrix and its inverse, allowing for retrieval
##and computation of the inverse only when necessary (to save computational time)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
##created by makeCacheMatrix. If the inverse is already cached, it retrieves the
##cached value to avoid redundant computation otherwise it calculates the 
##inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
        ## Return a matrix that is the inverse of 'x'
  inv
}
