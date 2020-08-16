## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getInverse()
  if(!is.null(invert)){
    message("Cached data is received")
    return(invert)
  }
  matrix <- x$get()
  invert <- solve(matrix,...)
  x$setInverse(invert)
  invert
}
