## The pseudo-type cachematrix provides a wrapper around a matrix which allows
## for caching a calculated inverse. The calculation of the inverse is performed
## by the 'cacheSolve' function, which will return the cached version instead
## of calculating it if the inverse has already been calculated and cached.

## Creates a wrapper around a matrix that will cache the calculated inverse.

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


## Calculates the inverse of a cache matrix (i.e., a matrix wrapped by the 
## makeCacheMatrix function). This function uses the cached version of the
## matrix if it exists, or calculates and caches it if doesn't.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
