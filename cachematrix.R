# This script contains functions to create a matrix that can cache its inverse
# after that is computed to save processing time later.

# This function creates an object that can handle caching a matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        # This function overwrites the matrix and resets its cached inverse.
        # Clearing the cached inverse prevents getInv() from returning outdated
        # data.
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        # This function returns the matrix
        get <- function() {x}
        
        # This function caches the matrix's inverse
        setInv <- function(inverse) {inv <<- inverse}
        
        # This function returns the matrix's inverse
        getInv <- function() {inv}
        
        #Return a list of the above functions so they can be called elsewhere
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# This function takes a CacheMatrix, determines the inverse, and caches the
# result before returning it. CacheSolve assumes that the matrix is solvable.

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
