## makeCacheMatrix() applies a particular environment to a matrix 
## and cacheSolve() returns the inverse of that matrix, 
## either by calculating it (if never calculated before) 
## or by retrieving it from memory (if previously calculated)

## makeCacheMatrix() defines set(), get(), setinv(), and getinv() functions
## and names them for use within other functions

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(get = get, set = set, setinv = setinv, getinv = getinv)
}


## cacheSolve() returns the inverse of matrix 'x', 
## after first checking memory to see if it was previously returned

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     invmatrix <- x$get()
     inv <- solve(invmatrix, ...)
     x$setinv(inv)
     inv
}