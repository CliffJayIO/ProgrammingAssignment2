## Two functions that (1) create a 2x2 matrix that can cache itself
## (2) create a cached function of that inverse

## First, makeCacheMatrix creates a 2x2 matrix that can cache itself
## The end product is actually 4 functions within a list that respectively:
## set matrix, get matrix, set inverse of matrix, get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        ## First element is a function that sets the matrix
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        ## Second element is a function that gets the matrix
        get <- function() x
        ## Third element is a function setting the inverse of the matrix
        setinverse <- function(solve) v <<- solve
        ## Fourth element is a function getting the inverse of the matrix
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve works with makeCacheMatrix code, retrieving cached inverse
## or calculating and storing the inverse if it hasn't already been stored
cacheSolve <- function(x, ...) {
        ## Set v as the contents of getinverse defined above
        v <- x$getinverse()
        ## If getinverse already contains the inverse, return cached inverse
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        ## Otherwise use the solve function to store the inverse within getinverse
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
}