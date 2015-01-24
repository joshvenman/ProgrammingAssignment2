## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: create a special "matrix" that contains functions to set and get the value 
## of the matrix and set and get the value of the inverse of the matrix passed

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        getenv<- function() environment()
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv,
             getenv = getenv)
}


## cacheSolve: get the inverse of the passed matrix unless the passed matrix is unchanged from the previous call
## if it has not changed, use the cached inverse value. If it has changed, compute the new inverse

cacheSolve <- function(x, ...) {

        m <- x$getinv()
        env <- x$getenv()
        data <- x$get()
        matrixChanged = FALSE

        # if no current saved last matrix then make it an emoty matrix and force solve()
        if(!exists("lastmatrix")) {
                lastmatrix <- matrix()
                matrixChanged <- TRUE
        }

        # Has the source matrix changed?
        if(identical(lastmatrix, data)) {
                # Matrix has not changed - get existing
                message("Matrix has not changed")
                matrixChanged <- FALSE
        } else {
                message("Matrix has changed")
                matrixChanged <- TRUE
        }
        
        if(!is.null(m)) {
                # Have an existing cached inverse so if matrix has not changed, use this
                if(!matrixChanged) {
                        message("Getting cached inverse")
                        return(m)
                }
        }
        
        m <- solve(data, ...)
        x$setinv(m)
        # Persist this matrix for comparison next time
        lastmatrix <<- data
        m
}
