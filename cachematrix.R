# This script contains functions to create a matrix that can cache its inverse
# after that is computed to save processing time later.


# This function creates an object that stores both a marix and can cache its
# inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize variables for the input matrix and its inverse.
        # We need a variable for the input matrix because otherwise the cache
        # matrix will look for "x" in the parent environment and point to that.
        
        # Normally, the parent environment where x is defined is the global env.
        # This is a problem because if we write to the same name then the
        # input matrix will be overwritten with a list of functions.
        # E.g. data <- makeCacheMatrix(data) causes this problem.
        
        # Storing the matrix at the local level within the makeCacheMatrix
        # environment solves this problem by eliminating the problematic
        # manipulation and linking to the value of x in the parent environments.
        
        mat <- x
        inv <- NULL
        
        # This function overwrites the matrix and resets its cached inverse.
        set <- function(y){
                # y is stored in the mat variable in the parent environment, ie.
                # makeCacheMatrix.
                mat <<- y
                
                # inv is cleared in the parent environment since the old inverse
                # is presumably out of date.
                inv <<- NULL
        }
        
        # This function returns the stored matrix
        get <- function() {mat}
        
        # This function caches the matrix's inverse in the makeCacheMatrix
        # environment.
        setInv <- function(inverse) {inv <<- inverse}
        
        # This function returns the stored matrix's inverse
        getInv <- function() {inv}
        
        # Return a list of the above functions so they can be called thru the
        # cache matrix.
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# This function takes a cache matrix, determines the inverse, and caches the
# result in the cache matrix before returning it. CacheSolve assumes that the
# matrix is solvable and square.

cacheSolve <- function(x, ...) {
        
        # Retrieve the inverse matrix stored in x
        inverse <- x$getInv()
        
        # Return the inverse matrix if it has already been cached in x
        if(!is.null(inverse))
        {
                message("Retrieving cached data")
                return(inverse)
        }
        
        # Determine the inverse matrix and cache it if this hasn't been done yet
        inverse <- solve(x$get())
        
        # Cache the inverse in x
        x$setInv(inverse)
        
        #Return the inverse matrix
        inverse
}
