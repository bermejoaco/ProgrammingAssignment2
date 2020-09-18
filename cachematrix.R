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