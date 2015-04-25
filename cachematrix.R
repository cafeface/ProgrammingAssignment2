## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" that is really
## a list of functions to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve retrieves or calculates the inverse of 
## a "cache matrix" created with makeCacheMatrix.
## It first checks whether the inverse is already cached.
## If so, it returns the saved value.  Otherwise, it
## calculates the inverse and caches it by invoking
## setinverse.
cacheSolve <- function(x = numeric(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
