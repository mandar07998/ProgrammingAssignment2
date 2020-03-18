## These functions together retrieve the inverse of a "matrix" list created and stored by the first function
## But first, the second function checks to see if an inverse had already been calculated and stored. If so,
## it retrieves the inverse. If not, it calculates the inverse of the matrix fed into the function.

## This function creates a "matrix", which is really a list that caches the inverse of the matrix.
## It sets the value of the matrix, gets the value of the matrix, sets the value of the inverse, and 
## gets the value of the inverse.

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


## This function retrieves the inverse of a matrix from the previous function. However, if no inverse has been 
## cached, it calculates the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
