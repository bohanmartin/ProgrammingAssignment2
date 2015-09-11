## Hello! This pair of functions should be useful for calculating
## the inverse of a matrix. Together, they will calculate the
## inverse of a matrix, or, in the case that the calculation has
## already been performed, simply return that solution.
## This is meant to save the time that would have been used by
## repeating this potentially long calculation.


## makeCacheMatrix creates the matrix object that can cache its 
## inverse.

makeCacheMatrix <- function(x = numeric()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that either calculates the inverse of the
## matrix entered in makeCacheMatrix, or, if the inverse of that
## matrix has already been calculated, cacheSolve returns that
## solution without re-calculating.

cachesolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}