## Allows creation of a matrix that can cachce its own inverse
## This caching can save costly computation timee if the matrix
## is repeatedly attemting to compute its inverse.

## Takes a matrix and creates a list that allows the matrix
## to be retrieved or updated.  The list also provides the ability
## to retrieve or set the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Creating new cache matrix, clearing inverse
    i <- NULL
    
    set <- function(y) {
        ## Assign to new matrix
        x <<- y
        
        ## Clear cached inverse as it has not yet been calculated
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Creates inverse of matrix.  If inverse has been previously calculated,
## returns cached inverse value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
