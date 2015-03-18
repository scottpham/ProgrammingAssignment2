##These two functions create, set, and update matrices with cached inverse values.

## Caches the inverse of an input matrix. Provides functions for getting the cached value of the inverse, and for setting a new cached value.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setInverse <- function(source) inv <<- source
    getInverse <- function() inv
    list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of an invertible matrix. Checks for cached value first and returns that cached value if it exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    x<- source(data, ...)
    x$setInverse(inv)
}
