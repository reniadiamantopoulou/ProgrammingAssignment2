## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## This pair of functions will cache the inverse of a matrix!!!!!


## This function will creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmat <- function(inverse) inv <<- inverse
    getmat <- function() inv
    list(set = set , get = get, setmat = setmat, getmat = getmat)
}


## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix above.
## If the inversi has been already calculated and the matrix has not changed then the function below (cacheSolve) should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getmat()
        
        ## Checks if the inverse has already been calculated and if the matrix has not changed
        ## if the two conditions above are TRUE it retrieves the inverse stored in cached data.
        if(!is.null(inv))
            message ("the matrix has not changed , get cached data")
            return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        inv
}
