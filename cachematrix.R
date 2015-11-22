## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix returns a list of functions
## set, get, setinv and getinv are the functions
## set and get are used to set the value of an invertible matrix or get the same
## setinv and getinv are used to calculate the inverse of the matrix set by set
## cacheSolve function will accept one of the special matrices created by makeCacheMatrix
## as input and will check if the inverse of this matrix is already set
## if not it will calculate the inverse of the matrix and cache it and will finally return the inverse
## of the matrix

## Write a short comment describing this function
## Function to create a list of functions that can be used to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inv) {
    xinv <<- inv
  }
  getinv <-function() {
    xinv
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Function to return the inverse of a matrix
## The returned matrix is either cached data or calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
