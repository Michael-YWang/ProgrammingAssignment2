## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is used to create the initial data structure and to cache and retrieve the matrix and its inverse. 

## Write a short comment describing this function
# Given an initial matrix, this method returns a special wrapper, in the form of a list, with functions that can be used to:
# - set the underlying matrix again
# - get the underlying matrix
# - set the inverse of the underlying matrix (which caches this value internally)
# - get the inverse of the underlying matrix (which uses the cached value)



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)   
        
}


## Write a short comment describing this function
## cacheSolve calculates matrix inverse of the x matrix. This function
## first checks if there is a cached inverse. If there is a cached inverse then
## cacheSolve returns the cached value immediately. If there is no cached inverse 
## it computes the inverse first, stores it in the cache, and returns the inverse value.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
