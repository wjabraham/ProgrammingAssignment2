## R functions to cache potentially time-consuming computation of calculating
## the inverse of a matrix. For example, taking the inverse of a numeric 
## matrix is typically a fast operation. However, for a very large matrix,
## it may take too long to compute the inverse, especially if it has to be
## computed repeatedly, such as in a loop). If the contents of a matrix are 
## not changing, it may make sense to cache the value of the inverse so that
## when we need it again, it can be looked up in the cache rather than be
## recomputed.

## makeCacheMatrix creates and returns a list of functions that:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the matrix inverse
##   - get the value of the matrix inverse
##
makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse to NULL
    inverseMatrix <- NULL
    
    ## set value of original (non-inverted) matrix into global environment 
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
        
    ## get the value of the original matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setsolve <- function(solve) inverseMatrix <<- solve
    
    ## get the inverse of the matrix
    getsolve <- function() inverseMatrix
    
    ## create the list of matrix functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Calculate the inverse of the matrix created with the above function.
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of
## the inverse in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get cached matrix
    inverse <- x$getsolve()
    
    ## if a cached matrix is found, return it
    if (!is.null(inverse)) {
        # return cached matrix
        message("getting cached data")
        return(inverse)
    }
    
    ## cached matrix was not found, so the inverse needs to be calculated
    ## get original (non-inverted) matrix
    data <- x$get()
    
    ## calculate inverse using solve function
    inverse <- solve(data, ...)
    
    ## cache inverse
    x$setsolve(inverse)
    
    # return value
    inverse
}
