## The purpose of this script is to cache the inverse of a matrix in order to avoid costly computation

## 1. Create a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function (solve) i <<- solve
    getinverse <- function() i
    list (set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
    
}


## 2. Calculate the inverse of the matrix returned by the function above.
## If the inverse was already calculated, it will be retrieved from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get ()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}