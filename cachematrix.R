## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. The purpose of the following two functions
## is to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set the value of the matrix
    set<-function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() {
        return(x)
    }
    
    ## Set the value of the inverse operation (using solve function) 
    ## on the matrix
    setInverse <- function(solve) {
        m <<- solve
    }
    
    ## Get the value of the inverse operation (using solve function) 
    ## on the matrix
    getInverse <- function() {
        return(m)
    }
    
    ## Constructor of the list containing the function previously defined
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the function should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Look for inverse in the cache
    m <- x$getInverse()
    
    ## If inverse exists in the cache, return it
    if(!is.null(m)){
        message("Getting cached data")
        return(m)
    }
    
    ## Inverse doesn't exist in the cache, need to calculate it using 
    ## solve function working for square invertible matrix
    matrix <- x$get()
    m <- solve(matrix, ...)
    
    ## Add inverse matrix in the cache
    x$setInverse(m)
    
    ## Return the inverse matrix
    return(m)
}
