## This function is created for caching the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtrx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtrx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## The purpose of this function is to get the inversed matrix (to calculate or to get from cache)

cacheSolve <- function(x, ...) {
    inverse <- mtrx$getinv()
    if(!is.null(inverse)) {
        message("The matrix from the cach is getting now...")
        return(inverse)
    }
    data <- mtrx$get()
    invserse <- solve(data, ...)
    mtrx$setinv(inverse)
    return(inverse)
}
