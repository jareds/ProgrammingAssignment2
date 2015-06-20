## makeCacheMatrix creates a function that stores a matrix for use as an argument
## to cacheSolve cacheSolve finds the inverse of a matrix created with
## makeCacheMatrix

## makeCacheMatrix returns a function with a reference to the matrix passed as an
## argument The return value of this function will be used as an argument to
## cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # Set return value to NULL
    # Functions to get and set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv = function(inverse) m <<- inverse
    getinv = function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve finds the inverse of a matrix created with makeCacheMatrix If the
## inverse has already been found it is returned from a cache Else it is
## calculated and then returned.
cacheSolve <- function(x, ...) {
    m = x$getinv()
    # If not null we already calculated the inverse so return it
    if (!is.null(m)) {
        return(m)
    }
    # m was not null so find inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    return(m)
} 
