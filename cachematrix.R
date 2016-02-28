## This source file contains two functions 'makeCacheMatrix' and 'cacheSolve'.
## 'makeCacheMatrix' creates a special matrix object that can cache its inverse.

## 'makeCacheMatrix' takes a traditional matrix x and returns a list
## containing four elements:
## 1. a function 'get' which returns the value of the matrix x unless reset with 'set'
## 2. a function 'set' which assigns a new value to the matrix returned by 'get'
## 4. a function 'getinv' which returns a cached value of the inverse matrix
## 3. a function 'setinv' which assigns a new value to the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function 'cacheSolve' takes a matrix x, created with 'makeCacheMatrix' and
##      - evaluates and caches its inverse if x$getinv() is NULL
##      OR
##      - returnes the cached inverse if it has been computed before and the
##        matrix not changed

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
