## Put comments here that give an overall description of what your
## functions do

## This function will create the "matrix" object that we want the inverse of. It will
## also cache its inverse, if already calculated (I hope).

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, 
     get = get, 
     setinverse = setinverse, 
     getinverse = getinverse)
}


## This function solves the inverse of the above matrix, or returns it from the cache
## if already calculated!

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
