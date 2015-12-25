## makeCacheMatrix creates a list that holds a matrix
## and its inverse in a "cache".

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks to see if a matrix and its inverse are
## already in the cache. If they are, it does not recalculate
## and instead returns the inverse data from the cache. If
## they are not, it uses the solve() function to calculate
## the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
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

