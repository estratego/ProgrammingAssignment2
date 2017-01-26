## The makeCacheMatrix function will first set the matrix, solve it's inverse and cache the result.
## Then 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setinverse <- function(solve) {m <<- solve}
        getinverse <- function() {m}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve will check if a value is already attributed to m (and then just recover it without
## losing computational time) or calculate it in case it doesn't exist.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}