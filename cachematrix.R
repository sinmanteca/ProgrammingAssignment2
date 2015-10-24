## Creates a vector that includes the following functions:
## set - stores the input matrix and sets the global variable m to NULL
## get - retrieves the stored matrix
## setinverse - stores the input inverse matrix (to be provided by cacheSolove function)
## getinverse - retrieves the stored input matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Creates a vector that calculates the inverse of the stored matrix.
## But before it does this, it first checks to see if the inverse matrix has already
## been created. If x$getInverse() is not NULL, then the inverse matrix that is stored
## in the global variable, m, is used instead of calculating the inverse matrix again.

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
