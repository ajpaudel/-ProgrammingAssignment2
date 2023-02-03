## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()
                x
        setInverse <- function(inverse)
                inv <<- inverse
        getInverse <- function()
                inv
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


# This function creates a special "matrix" object that can cache its
# inverse. It takes a matrix as an input and stores the matrix and its
# inverse in a list, which is then returned as the "matrix" object.
# The inverse will only be calculated once and subsequently retrieved
# from the cache.





cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
