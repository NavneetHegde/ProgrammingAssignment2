## This twin function helps compute and cache the inverse of matrix ##

## This function exposes functionality to get/set the matrix and read/write
## cache using lexical scoping.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    # reset the cache variable everytime matrix is set
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # read the matrix
    get <- function() x
    
    # set the cache variable with the data
    setMatrixInverse <- function(matrixInverse){
        m <<- matrixInverse
    }
    
    # read the cached data variable.
    getMatrixInverse <- function() m
    
    # exposed functionality of the function
    list(set= set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)

}


## This function returns an inverse of a matrix passed as the argument x It is
## assumed that matrix supplied is square invertible matrix
cacheSolve <- function(x, ...) {
    
    # read the cached matrix inverse 
    m <- x$getMatrixInverse()
    if(!is.null(m))
    {
        # returns cached inverse of a matrix
        message('getting cached matrix inverse')
        return(m)
    }
    
    # read the matrix
    data <- x$get()
    
    # compute inverse of matrix
    m <- solve(data)
    
    # set the cache of matrix inverse
    x$setMatrixInverse(m)
    m
}
