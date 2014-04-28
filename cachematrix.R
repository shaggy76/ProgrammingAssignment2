## Created By: Scott Harrison
## Created On: 4/27/2014
## Last Updated: 4/27/2014
##
## These two functions cache a copy of the inverse of a matrix to be accessed laster.  
## makeCacheMatrix returns a matrix that holds functions that can find if there is an 
## inverse matrix stored in memory.  The cacheSolve sets the inverse either from memory 
## or calculates it if it does not exist.

## makeCacheMatrix stores four functions in a list, set(), get(), setinv() and getinv().
## These functions access a cached inverse matrix if it exists.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}


## cacheSolve returns the inverse matrix which is either stored in memory or it calculates it if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m)
        x$setinv(i)
        i
}
