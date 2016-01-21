##############################################################################  
## Author: Sachin Monteiro.
## Dated : 21-Jan-2015
############################################################################## 
## Assignment: Caching the Inverse of a Matrix.
## Matrix inversion is usually a costly computation and there may be some  
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss 
## here). Your assignment is to write a pair of functions that cache the inverse 
## of a matrix. 
##############################################################################
## Write the following functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
##    cache its inverse.
##############################################################################
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
##############################################################################
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been 
##    calculated (and the matrix has not changed), then the cachesolve should
##    retrieve the inverse from the cache.
## Compute the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, then solve(X) returns 
## its inverse.
##############################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
