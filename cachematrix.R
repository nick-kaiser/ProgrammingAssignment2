## These two functions (makeCacheMatrix and cacheSolve) enable caching of matrix
## inversions under the assumption that the input array is invertable.

## This function creates a special "vector" (really a list) of functions that 
## can set and return a matrix, and can also store and return the inverse
## of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function will first check to see if the inverse of a matrix has already been
## calculated and cached. If it has, it will return the cached value. If not, it will
## calculate the inverse, cache it, and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
