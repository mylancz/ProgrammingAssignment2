## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This functions cache matrix data

makeCacheMatrix <- function(x = matrix()) {
  inverseOfMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseOfMatrix <<- NULL
  }
  get <- function() x
  setinversion <- function(solve) inverseOfMatrix <<- solve
  getinversion <- function() inverseOfMatrix
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## Write a short comment describing this function
## function compute inversion of matrix through cached object

cacheSolve <- function(x, ...) {
  inverse <- x$getinversion()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinversion(inverse)
  inverse
}
