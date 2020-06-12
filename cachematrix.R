## Put comments here that give an overall description of what your
## functions do

## The following function returns a list of functions that are defined to work 
## on a matrix
## for example makematrix <- makeCacheMatrix()
##then use the list of functions as makematrix$...

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the list created by above function as an input
## The list must be defined beforehnad and must be set to a value
## for example: makematrix$set(matrix(c(1:4), nrow = 2, ncol = 2))
## then call the below function as cacheSolve(makematrix)

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
