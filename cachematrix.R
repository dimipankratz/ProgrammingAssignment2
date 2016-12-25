## Functions that calculate the inverse of a matrix a caches the result.

## Creates a matrix wrapper with a cache for the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse for a given matrix wrapper or reads the result from cache, if it is already available
cacheSolve <- function(x, ...) {
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

## Example
## x <- rbind(c(2, 5), c(1, 3))
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
##      [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
## > cacheSolve(m) %*% m$get()
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1