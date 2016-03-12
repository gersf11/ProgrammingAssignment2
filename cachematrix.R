## These functions are used to create a special object "matrix"
## that can chache its inverse

## Creates a special object "matrix"
## This object has the following functions:
## set: sets member m_mat (m_mat must be invertible)
## get: gets member m_mat
## setInverse: sets the inverse of m_mat
## getInverse: returns the inverse of m_mat

makeCacheMatrix <- function(m_mat = matrix()) {
    inv <- NULL
    set <- function(p_mat) {
        m_mat <<- p_mat
        inv <<- NULL
    }
    get <- function() m_mat
    setInverse <- function(p_inv) inv <<- p_inv
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Creates cache of inverse of object x
## Object x has to be created using makeCacheMatrix

cacheSolve <- function(p_mat, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- p_mat$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- p_mat$get()
    inv <- solve(data, ...)
    p_mat$setInverse(inv)
    inv
    
}
