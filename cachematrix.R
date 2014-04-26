## These functions create a matrix, calculate the inverse of that matrix, 
## and then cache that inverse for future use.

## makeCacheMatrix creates a matrix, and also estabilshes various matrix related functions

makeCacheMatrix <- function(x = matrix()) {
        mtrx <- NULL
        set <- function(y){
                x <<- y
                mtrx <<- NULL
        } ## set can create a new matrix
        get <- function() x ## get displays the current matrix, if there is one
        setInverse <- function(matrix) mtrx <<- matrix
        getInverse <- function() mtrx
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix and then caches the reuslts
## this saves processing time when the inverse of the matrix is needed in the future

cacheSolve <- function(x, ...) {
        mtrx <- x$getInverse()
        if(!is.null(mtrx)){
                message("getting cached data")
                return(mtrx)
        } ## if the matrix has already been cached, this returns the cached matrix
        data <- x$get() ## this brings the matrix into the function to use
        mtrx <- solve(data) ## this finds the inverse of the matrix
        x$setInverse(mtrx) ## this establishes the cached matrix
        mtrx
}