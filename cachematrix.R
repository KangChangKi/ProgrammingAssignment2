## It provides 'special matrix' object which can cache time-consuming calculation of inverse of the matrix.

## It creates an object which can has a matrix and an inverse of the matrix inside.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(m) {
                x <<- m
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}

## If the special matrix object has the cached inverse, it returns the inverse.
## Otherwise it calculates the inverse and cache that into the object, and return the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
