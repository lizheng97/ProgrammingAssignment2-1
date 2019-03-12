## Caching the inverse of a matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { ## If the matrix is modified, the inverse value is set to null
                             ## so that the next time we call cacheSolve, the inverse is recalculated
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ## returns the matrix stored at the cacheMatrix
        setinverse <- function(inverse) inv <<- inverse  ## stores the inverse value
        getinverse <- function() inv                     ## compute the inverse of the stored matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse
        if(!is.null(inv)) {     ## If the inverse is already calculated, it's taken from the cacheMatrix
                message("getting cached data")
                return(inv)
        }
        ## If the inverse is not calculated
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
}
