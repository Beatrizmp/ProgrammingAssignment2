## With these functions, you can calculate the inverse of a matrix and, if the matrix does not changed, 
## retrieve the inverse of the matrix without having to recalculate it.

## makeCacheMatrix function sets the value of the matrix and its inverse, and get the value of both of them.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve function calculate the inverse of a given matrix and set the inverse of the matrix trough the setinverse function.
## If the inverse has been calculated previously then cacheSolve recovers the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
