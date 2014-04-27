# makeCacheMatrix creates a special matrix which will cache its inverse.
#   inv - the cached inverse
#   setinv(inverseData) - sets the inverse matrix
#   getinv() - returns the inverse matrix
#   get() - returns the matrix
#   set(originalData) - sets the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMat) inv <<- invMat
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#cacheSolve computes the inverse of a special matrix in case it was not already calculated and caches it.
#In case it was calculated previously, it will simply return the cached inverse.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv   
}
