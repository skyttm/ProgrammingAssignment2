## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        setinverse <- function(solve) s <<- solve
        ## get the value of the inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache. Otherwise, it calculates the inverse of the matrix and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## look up in the cache
        s <- x$getinverse()
        ## the inverse has already been calculated
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## not in the cache, recompute
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        ## Return a matrix that is the inverse of 'x'
        s
}
