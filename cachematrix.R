## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initializing the inverse as null
  invert <- NULL
  #Creating a set function to set inverse and x
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  #Creating a get function to get the value of x
  get <- function()x
  #Using a set function to set inverse
  setInverse <- function(inverse) invert <<- inverse
  #Using a get function to get inverse
  getInverse <- function() invert
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Getting the invesrse value if it is already in cache
  invert <- x$getInverse()
  #for the case that the inverse is not null and already in cache
  if(!is.null(invert)){
    message("Cached data is received")
    return(invert)
  }
  #setting the maxtrix with the value of x
  matrix <- x$get()
  #Calculating the inverse of matrix
  invert <- solve(matrix,...)
  #Settting the inverse of matrix in the value of x
  x$setInverse(invert)
  invert
}
