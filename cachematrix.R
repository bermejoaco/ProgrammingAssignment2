# Defines a "matrix" which is actually a list with 4 different
# functions: get, set, getinv, setinv.

makeCacheMatrix <- function(A = matrix()) {
    inv <- NULL
    set <- function(B) {
        A <<- B
        inv <<- NULL
    }
    get <- function() A
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# Computes inverse of the matrix created through makeCacheMatrix.

cacheSolve <- function(A, ...) {
    inv <- A$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- A$get()
    inv <- solve(data, ...)
    A$setinv(inv)
    inv
}