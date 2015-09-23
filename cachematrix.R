## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #set <- function(y) {
        #        message("in set")
        #        x <<- y
        #        m <<- NULL
        #}
        get <- function() {
                message ( " in get")
                x
        }
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        # list(set = set
        list(get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
