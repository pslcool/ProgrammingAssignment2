## Put comments here that give an overall description of what your
## functions do

## There are two functions: makeCacheMatrix; cacheSolve

## The following function consists of four functions

makeCacheMatrix <- function(x = matrix()) {
        ## initialization x as NULL
        matrix_inverse <- NULL
        
        ## function to set the matrix
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        
        ##function to get the matrix
        get <- function() x
        
        ##inverse of the matrix
        setInverse <- function(inv) matrix_inverse <<- inv
        
        ##function to get the inverse of the matrix
        getInverse <- function() matrix_inverse
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function is used to get cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getInverse()
        
        ## Checking if matrix_inverse is NULL
        if (!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        
        data <- x$get()
        
        ## calculate the value of the matrix's inverse
        matrix_inverse <- solve(data, ...)
        x$setInverse(matrix_inverse)
        matrix_inverse
}