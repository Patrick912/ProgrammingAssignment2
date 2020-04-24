## This package contains two functions.
## makeCacheMatrix creates a matrix which can cache an arbtrary result. In our
## assignment this will be the inverse of the matrix, but it could be use to 
## to cache different things.
## cacheSolve will invert a matrix and makes use of the "cache matrix" to be
## efficient if it is called more than once for the same "cache matrix"

## Creates a list of functions with a matrix x as base
## The functions allow to 
## "getMatrix" get the matrix
## "setMatrix" set/overwrite the matrix (not really needed for assignment)
## "getResult" get the cached result
## "setResut" cache the result
## Basically this allows to cache an arbitrary result/object for a given matrix
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    #important to remove the cache value whe the matrix changes
    set <- function(y) {
        x <<- y
        result <<- NULL
    }
    get <- function() x
    setResult <- function(data) cache <<- data
    getResult <- function() cache
    list(setMatrix = set, getMatrix = get,
         setResult = setResult,
         getResult = getResult)
}

## This function makes use of the "cache matrix" that is created by function 
## makeCachematrix
## It will check if there is already a cached result and return it in case
## it is available. Otherwise it will invert the matrix, store the result in the
## cache and return the inverted matrix
cacheSolve <- function(x, ...) {
    inverse <- x$getResult()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setResult(inverse)
    inverse
}