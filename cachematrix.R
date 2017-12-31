## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix creates a list of 4 functions
## to set and get the inverse of a matrix, x
## The assignment allows for the assumption that the matrix x 
## is invertible so no error handling is included

makeCacheMatrix <- function(x = matrix()) {
    # Reset the matrix inverse
    xinv <- NULL
    # Function to set the value of the matrix 
    # And reset the value of the inverse of the matrix to NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    # Function to get the value of the matrix
    get <- function() x
    # Function to set the value of the inverse of the matrix
    setinv <- function(i) xinv <<- i
    # Function to get the value of the inverse of the matrix
    getinv <- function() xinv
    # Create the list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve
## Checks for a current value for the inverse of the matrix
## If xinf has a value, then return it
## If not, calculate and set itlibrary(swirl)library(swirl)library(swirl)

cacheSolve <- function(x, ...) {
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    # If not, then calculate and set it
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
} 