## These functions show the possibilities provided with so-called "closures",
#i.e., functions, written by other functions within themselves.

## This function creates a "matrix"-like cache object, which is capable of 
## storing the inverted version of itself

makeCacheMatrix <- function(x = matrix()) {
    ## Setting the "default" (not exactly) values for input matrix
    inversion <- NULL
    set <- function(y) {
        x <<- y
        inversion <<- NULL
    }
    ## Getting the input matrix
    get <- function() x
    ## Inverting the input matrix
    setInverse <- function() inversion <<- solve(x)
    ## Returning the inverted matrix
    getInverse <- function() {
        inversion
    }
    ## Creating a list of parameters
    list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)  
}

## This function checks whether the cache of matrix inversion from the previous
## step is still exists, and, if so, return the CASHED (non-recomputed) value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversion <- x$getInverse()
    ## Checking whether matrix has been already computed and it has no changes
    if (!is.null(inversion)) {
        message("getting cached matrix")
        return(inversion)
    }
    ## Getting the matrix
    matrix <- x$get()
    ## Inverting and setting the inverted matrix
    inversion <- solve(matrix, ...)
    x$setInverse(inversion)
    ## Return inverted matrix
    inversion
}
