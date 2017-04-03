## Put comments here that give an overall description of what your
## functions do

## Creates a matrix with state and methods
## Use the methods set, get, setinverse and get inverse to interact with
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x'
## If the solution has already been calculated and is cached, cacheSolve will
## use the cached solution. Otherwise, it will calculate the inverse, cache it
## and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
