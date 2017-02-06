## Caching the inverse of a matrix:
## Matrix inversion is usually a costly computation and there could be benefits to caching the inverse of a matrix instead of computing it repeatedly
## This assignment includes a pair of functions- one to create a special "matrix" object and the other to cache the inverse of this matrix

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

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
        

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}

 