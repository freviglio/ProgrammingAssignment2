## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly. 
## Here we write a pair of functions that cache the inverse of a matrix



##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##  input > an invertible matrix
        ##  out > a list containing functions (set/get matrix, set/get inverse) 
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinvs <- function(inverse) invs <<- inverse 
        getinvs <- function() invs
        list(set=set, get=get, setinvs=setinvs, getinvs=getinvs)
}



## Calculate the inverse of the matrix returned by makeCacheMatrix(). Whether the inverse has been already calculated this function get the inverse directly from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## input > output of makeCacheMatrix()
        ## output >inverse of matrix input to makeCacheMatrix()
        
        invs = x$getinv()
        
        # inverse already calculated
        if (!is.null(invs)){
                # getting from cache
                message("getting cached data")
                return(invs)
        }
        
        # inverse not already calculated 
        mdata = x$get()
        invs = solve(mdata, ...)
        
        # add invs to cache
        x$setinv(invs)
        
        return(invs)
}