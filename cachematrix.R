## These functions computes the inverse of an invertible matrix.


## makeCacheMatrix() takes an invertible matrix as an input and set
## the value of its inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) i <<- Inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function perorms matrix inversion. If inputted matrix from above
## function was already computed then result will be returned from cache 
## using "get" else result will be compuetd and resulted will be stored 
## in cache using "setInverse" for future computation.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inversed matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
