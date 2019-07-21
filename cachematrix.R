## makeCacheMatrix() and cacheSolve() are functions to cache the inverse of a
## matrix and retrieve the cached value

## makeCacheMatrix() creates an R object that can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    # set() allows setting a new matrix without initializing a new
    # makeCacheMatrix object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # get() returns the matrix stored in the function environment
    get <- function() x

    # setInverse() sets the inverse of the matrix through cacheSolve()
    setInverse <- function(solve) inv <<- solve

    # getInverse() returns the inverse matrix
    getInverse <- function() inv

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve() sets or retrieves the inverse of a matrix from the
## makeCacheMatrix() object's environment

cacheSolve <- function(x, ...) {

    # try to retrieve the inverse matrix from the makeCacheMatrix object
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # if no inverse matrix is cached, calculate and set the inverse matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
