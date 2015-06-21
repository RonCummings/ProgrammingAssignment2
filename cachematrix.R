## These two functions use R scoping to compute a matrix inverse
## the first time (when it is NULL) and use a cached version of
## the matrix inverse if it hasn't changed (doesn't need to be computed
## again)

## This function creates the storage space for the matrix and
## lists the functions that will be used on the matrix

makeCacheMatrix <- function(x = matrix()) {
    matx <- NULL
    set <- function(y) {
        x <<- y
        matx <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) matx <<- solve
    getinverse <- function() matx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## This function computes the inverse if it needs to be computed
## otherwise it returns the cached value of the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
       matx <- x$getinverse()
    if(!is.null(matx)) {
        message("getting cached matrix data")
        return(matx)
    }
    data <- x$get()
    matx <- solve(data, ...)
    x$setinverse(matx)
    matx
}
