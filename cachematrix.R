## A pair of functions used to create a matrix and cache it's inverse. 

## A function used to create a matrix, along with a few helper functions.

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


## A function used to find the inverse of a matrix. If it has already been calculated and stored in the cache, the calculation isn't performed again, thus saving time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

