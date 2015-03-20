##These two functions create matrices with methods to set, and update and cache its inversei.

makeCacheMatrix <- function(x = as.matrix()) {
    
	inv <- NULL
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    get <-function(){ x }
    setInverse <- function(new_inv) { inv <<- new_inv }
    getInverse <- function(){ inv }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)	
}


## Takes an invertible matrix and other paramaters. Returns the inverse
#of the invertible matrix. Checks for cached value first and returns that
#cached value if it exists.

cacheSolve <- function(x, ...) {
	#Get inverse if it was set already
    inv <- x$getInverse()
    #If the inverse was set, return a message and return the value of the 
	#inverse elment from the list x
	if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    #The get method returns the whole matrix, sans functions
	data <- x$get()
    #Calculate the inverse and return it along with ...
	inv <- solve(data, ...)
	#Use X's set method to cache the inverse 
    x$setInverse(inv)
}
