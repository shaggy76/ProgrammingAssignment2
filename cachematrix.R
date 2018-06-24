## Created By:  Scott Harrison
## Created On: June 24, 2018
## 
## These functions facilitate calculate the inverse of a matrix.  To make
## the calculation more efficient, cacheSolve() will look for a cached inverse
## solution before calculating the inverse again.  mackCacheMatrix() provides
## the functionality to store and retrieve a cached inverse solution to be
## retrieved later.

## makeCacheMatrix returns a list with four funtions to use to cache a matrix
## and its inverse.
## set() assigns the matrix.  
## get() returns the matrix.
## getInv() returns the inverse.
## setInv() assigns the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(m) {
        x <<- m
        Inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) Inv <<- inverse
    getInv <- function() Inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve returns the inverse of a matrix.  If there is a cached inverse
## it returns that.  If not, it computes the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if (!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    m <- x$get()
    inverse <- solve(m)
    x$setInv(inverse)
    inverse
}


# ## testing code
# 
# matrix <- rbind(c(0, -3, -2), c(1, -4, -2), c(-3, 4, 1))
# inverse <- solve(matrix)
# my_matrix <- makeCacheMatrix(matrix)
# my_matrix$getInv()
# my_inverse <- cacheSolve(my_matrix)
# my_inverse
# my_inverse <- cacheSolve(my_matrix)
# my_inverse
# my_matrix$getInv()
# 
# matrix2 <- rbind(c(1, 2), c(1, 1))
# inverse2 <- solve(matrix2)
# my_matrix2 <- makeCacheMatrix(matrix2)
# my_matrix2$getInv()
# my_inverse2 <- cacheSolve(my_matrix2)
# my_inverse2
# my_inverse2 <- cacheSolve(my_matrix2)
# my_inverse2
# my_matrix2$getInv()
# 
# my_matrix$getInv()
# my_matrix2$getInv()

