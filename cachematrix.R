## Functions to manipulate and cache matrix inversion operations
## This file contains a function that generates a cacheMatrix list object 
## that has functions to set and get chached values. This file has, as well,
## a function to canculate the matrix inversion value and store its value
## in a cacheMatrix object.
 

## This function recieves an invertible matrix and returns a list
## with getter and setter functions for the matrix and the inverted 
## matrix manipulation
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverted <- function(inverted) i <<- inverted
    getInverted <- function() i
    list(set = set, get = get,
         setInverted = setInverted,
         getInverted = getInverted)
}


## Recives a list object in the format returned from makeCacheMatrix function
## and if there is an inverted matrix value within this object, 
## return this cached value, otherwise, calculates the inverted matrix from the 
## matrix within the cacheMatrix object.
cacheSolve <- function(x, ...) {
    i <- x$getInverted()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverted(i)
    i
}