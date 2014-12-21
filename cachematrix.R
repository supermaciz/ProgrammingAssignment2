## These functions store in a cache the inverse of a matrix


## With the matrix "x", makeCacheMatrix creates a special matrix object
## with the ability to store its inverse. This object is actually a list
## of functions(to get and set the matrix and its inverse).

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    set <- function(y) {
        x <<- y
        solved <<- NULL
    }
    get <- function() x
    setsolve <- function(s) solved <<- s
    getsolve <- function() solved
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve returns the inverse of the special matrix object "x".
## It checks if the inverse is already computed. If not, cacheSolve computes it
## the "solve" function

cacheSolve <- function(x, ...) {
    s <- x$getsolve
    if (!is.null(s)) {
        message("getting cached data")
        s
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
