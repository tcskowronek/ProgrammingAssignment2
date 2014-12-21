# Coursera: John Hopkins University
# Data Science Track Specialization
# Course: R Programming
#
# Author: Thomas Skowronek
# Date: 12/21/2014
#
# Unit test data sourced from:
#   https://class.coursera.org/rprog-016/forum/thread?thread_id=140


makeCacheMatrix <- function(x = matrix()) {
    # Creates a special matrix object that can cache it's inverse
    #
    # Args:
    #   x: A n x n Matrix
    #
    # Returns:
    #   A list of functions to set and get the original matrix, as well as 
    #   functions to set and get the inverse of the matrix.
    
    m <- NULL  # used to cache the inverse of the matrix
    
    set <- function(y) {
        # Setter function to store the original matrix and clears the 
        # cached inverse of the matrix.
        
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        # Getter function that returns the origianl matrix
        
        x
    }
    
    setinverse <- function(inverse) {
        # Setter function to cache the inverse matrix
        #
        # Args:
        #   inverse: A matrix repersenting the inverse to the original matrix
        
        m <<- inverse
    }
    
    getinverse <- function() {
        # Getter function that returns the inverse matrix
        m
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
    # Calculates the inverse of the special matrix object returned by 
    # makeCacheMatrix().  Before doing so, it checks to see if the inverse 
    # matrix has been calculated.  If so, it returns the cached copy of the 
    # inverse matrix instead of recalculating it.
    #
    # Args:
    #   x: The special matrix objected created by makeCacheMatrix()
    #
    # Returns:
    #   A matrix that is the inverse of 'x'
    
    m <- x$getinverse()  # attempt to get the inverse matrix
    
    # Check if the inverse matrix exists and return it if it does
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()  # get the source matrix
    
    m <- solve(data, ...)  # calculate the inverse matrix
    
    x$setinverse(m)  # store the inverse matrix
    
    m
}
