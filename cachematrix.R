## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This functions cache matrix data

makeCacheMatrix <- function(x = matrix()) {
  inverseOfMatrix <- NULL
  set <- function(y) {
    x <<- y                 # Set matrix in parent environment
    inverseOfMatrix <<- NULL    # Reset inverse
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
  if(!is.null(inverse)) {             # Check if inverse matrix exists
    message("getting cached data")
    return(inverse)         # Return cached inverse
  }
  data <- x$get()
  inverse <- solve(data, ...)   # If no cached matrix, compute (by solve function)
  x$setinversion(inverse)
  inverse
}
