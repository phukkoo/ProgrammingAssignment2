#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly.
# Here we attempt to create a pair of functions 
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the
#inverse from the cache.

#For this assignment, we assume that the matrix supplied is always invertible.
#The first function, makeVector creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
              message("in set")
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        
        #Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        setinverse <- function(inv) invMatrix <<- inv
        getinverse <- function() invMatrix
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}
# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
# cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse
# in the cache via the setmean function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getinverse()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setinverse(invMatrix)
        invMatrix
}
