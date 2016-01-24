## Set of functions that uses caching to avoid recalculation of 
## the inverse matrix.
##
## Intended use:
## 1. Invoke makeCacheMatrix with the original matrix
## 2. Use resulting object with cacheSolve
## The inverse will only be calculated once.

## This function will return an object that is used for caching the 
## result of calculating the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Calculation function that utilizes objects created by makeCacheMatrix
## The inverse matrix will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}
