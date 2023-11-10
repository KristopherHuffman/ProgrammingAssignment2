
###################################################################################################

# File: cachematrix.R

# This file contains two functions:
    # (1) makeCacheMatrix(): a function that stores a matrix and its inverse
    # (2) cacheSolve(): a function that returns the inverse of a matrix, either cached or newly calculated

###################################################################################################


###################################################################################################

# Function: makeCacheMatrix()

# Use: stores a matrix and its inverse

# Input: a numeric matrix, x

# Returns: a list containing 4 functions (set(),get(),setinverse(), and getinverse())
    # Said in another way, makeCacheMatrix() returns a "makeCacheMatrix object" that stores a matrix
    # and its in inverse. This object also contains the defined behaviors or functions for objects of
    # the type "makeCacheMatrix".

###################################################################################################

makeCacheMatrix <- function(x = matrix()) { # default value of argument is empty matrix
    m <- NULL # create object and assign it NULL
    
    # Define function: set()
    set <- function(y) {
        x <<- y    # assign input y to x in the parent environment (the makeCacheMatrix() environment)
        m <<- NULL # assign NULL to m in the parent environment
    }
    
    # Define function: get()
    get <- function() x # return x from the parent environment
    
    # Define function: setinverse()
    setinverse <- function(inverse) m <<- inverse # assign input inverse to m in the parent environment
    
    # Define function: getinverse()
    getinverse <- function() m # return m from the parent environment
    
    # Return list containing 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

###################################################################################################


###################################################################################################

# Function: cacheSolve()

# Use: calculates (or retrieves from cache) the inverse of a matrix

# Input: a "makeCacheMatrix object", x

# Returns: m, the inverse of a matrix stored in the input makeCacheMatrix object. The returned
    # inverse is either calculated or retrieved from cache (if already previously calculated). 

# Notes: This code assumes the matrix stored in the input makeCacheMatrix object is invertible (i.e., has an inverse).

###################################################################################################

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse() # retrieve inverse from the input makeCacheMatrix object, x
    
    # if m is not NULL, there is a valid, cached inverse so we return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # get the matrix stored in the makeCacheMatrix object
    # can think of data as a temporary matrix that we only use inside this function
    data <- x$get() 
    
    m <- solve(data, ...) # calculate the inverse of the matrix
    
    x$setinverse(m) # set the inverse in the input makeCacheMatrix object to store in cache
    
    m # return the inverse
}

###################################################################################################





