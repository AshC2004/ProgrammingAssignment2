## The following functions are used to cache the inverse of a matrix.
## Matrix inversion is often a costly computation, so caching the result
## rather than recomputing it repeatedly improves performance.
##
## The `makeCacheMatrix()` function creates a special "matrix" object that
## can cache its inverse. The `cacheSolve()` function computes the inverse
## of this special matrix. If the inverse has already been calculated (and
## the matrix has not changed), it retrieves the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions:
## - set: to set the matrix
## - get: to get the matrix
## - setinverse: to cache the inverse of the matrix
## - getinverse: to retrieve the cached inverse (if available)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then it retrieves the inverse from the cache.
## Otherwise, it calculates the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
