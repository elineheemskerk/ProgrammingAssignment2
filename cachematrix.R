## The three functions below are designed to cache and retrieve the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve function retrieves the inverse from the cache.
## The function makeCacheMatrix can only be executed if the matrix supplied is square.
## The function checkSquare is being called to test if the matrix supplied is a square matrix.
## If not, this will result in an error meassage and the function makeCacheMatrix will not be executed
## We assume that the matrix supplied is invertible (i.e. determinant <>0)

checkSquare <- function(M) {
    if (nrow(M) == ncol(M)) {}
    else {stop ("error, your matrix should be square!")}  
}


## This function creates a special "matrix" object that can cache its inverse
## The object created contains 4 functions: set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
    checkSquare(x)
    Inv <- NULL
    set <- function(M){
        checkSquare(M)
        x <<- M
        Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) Inv <<- solve
    getInverse <- function() Inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInverse()
    if (!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInverse(Inv)
    Inv
}
