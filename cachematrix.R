## These functions are intended to be used to together to compute 
## and cache the inverse of an invertible matrix.  

## This function handles the storing and retrieval of a matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function works with the list object produced by 'makecachematrix'
## to compute and cache the inverse of the matrix stored within it.
## If the inverse of the matrix is already computed, it is retrieved from
## the list object rather than re-computed.

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
