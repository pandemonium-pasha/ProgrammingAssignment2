## Functions to make matrix that can cache its inverse value

## This function creates a matrix that can cache its inverse value
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    get <- function() x
    getInv <- function() i
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    setInv <- function(inv) i<<- inv
    list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## This function calculates the inverse of a matrix only if it has not already been
## calculated and cached, otherwise returns the cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if(!is.null(i)) {
        return (i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    i
}
