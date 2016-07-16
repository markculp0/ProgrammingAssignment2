## Project to create a special "matrix" object that 
## can cache it's inverse and retrieve inverse from
## cache.

## makeCacheMatrix : create a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list (set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## cacheSolve : compute the inverse of the special 
## "matrix" returned by makeCacheMatrix.  If already
## created, retrieve it from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    dat <- x$get()
    m <- solve(dat, ...)
    x$setmatrix(m)
}
