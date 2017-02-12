## Put comments here that give an overall description of what your
## functions do
## This functions allows to store the inversed matrix in cache and get it from cache,
## instead of recomputing every time

## Write a short comment describing this function
## This function creates an object that can cache the inverse of it

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function () inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
## This function retrieves cached version of matrix if it exists already
## and calculates the inverse if it is not in cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
        ## Return a matrix that is the inverse of 'x'
