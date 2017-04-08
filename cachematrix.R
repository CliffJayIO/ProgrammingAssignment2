## Two functions that (1) create a 2x2 matrix that can cache itself
## (2) create a cached function of that inverse

## Creates a 2x2 matrix that can cache itself
## The end product is actually 4 functions within a list that respectively:
## set matrix, get matrix, set inverse of matrix, get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) v <<- inverse
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Creates an inverse of a matrix, which is cached
## Works with makeCacheMatrix code
cacheSolve <- function(x, ...) {
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
}