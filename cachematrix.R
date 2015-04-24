# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinvrs <- function(inverse) invrs <<- inverse
    getinvrs <- function() invrs
    list(set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
}

#cacheSolve:compute the inverse of matrix.If already inverse calculated before,
#it returns the cached inverse

cacheSolve <- function(x, ...) {
    invrs <- x$getinvrs()
    if (!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    x$setinvrs(invrs)
    invrs
}