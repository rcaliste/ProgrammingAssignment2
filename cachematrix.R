## A set of functions that cache the inverse of a matrix


## A function that creates a special matrix that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## set the inverse property
        i <- NULL
        
        ## set the matrix
        set <- function(matrix) {
                x <<- matrix
                i <<- NULL
        }
        
        ## get the matrix
        get <- function() {
                x    ## returns the matrix
        }
        
        ## set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## get the inverse of the matrix
        getInverse <- function() {
                i     ## returns the inverse of the matrix
        }
        
        ## return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## A function that computes the inverse of the matrix 
## returned by makeCacheMatrix function.
## If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve 
## function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## return the inverse if it is already set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## get matrix from our object
        data <- x$get()
        
        ## calculate the inverse 
        m <- solve(data) %*% data
        
        ## set inverse to the object
        x$setInverse(m)
        
        ## return matrix
        m
}

        