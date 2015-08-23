# this function stores a martix and a caches the value of the inverse of the 
# matrix. It also Contains 4 sub-functions functions:
# - setMatrix      set the values of the matrix to a varible
# - getMatrix      returns the values of stored matrix
# - cacheInverse   compute the inverse of the matrix 
# - getInverse     get value of the cached inverse matrix 
#

makeCacheSolve <- function(x = matrix()) {
    
    inv <- NULL
    
    # set the values of the matrix to a varible
    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # returns the values of stored matrix
    getMat <- function() {
        x
    }
    
    # compute the inverse of the matrix 
    setinv <- function() {
        inv <<- solve(x)
    }
    
    # get value of the cached matrix
    getinv <- function() {
        inv
    }
   
    # list of the sub functions contained in the makeCacheSolve function
    list(setMatrix=setMat ,  getMatrix=getMat, CacheInverse=setinv , getInverse=getinv )
}


# This function calculates the inverse of a matrix created with 
# the makeCacheSolve function

cacheSolve <- function(m, ...) {
    # get the cached value 
    inverse <- m$getInverse()
    # if a cached value exists return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # else get the matrix and caclulate the inverse then store the value in
    # the cache
    data <- m$getMatrix()
    inverse <- solve(data)
    m$cacheInverse(inverse)
    
    # return the inverse of the matrix
    inverse
}
