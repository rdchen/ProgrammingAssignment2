## These two functions allows users to cache the inverse of a matrix, so that
## when they need it again, they can look it up in the cache instead of
## have to recompute it

## makeCacheMatrix function create a matrix object, which is really a list with the
## following functions:
##  1. Set a matrix to the object
##  2. Get the matrix
##  3. Set the matrix's inverse
##  4. Compute the inverse of the matrix
##  5. Return a special list of functions inside of makeCacheMatrix

makeCacheMatrix <- function(x=matrix()){
    x_inverse <- NULL
    set <- function(y){
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) x_inverse <<- inverse
    get_inverse <- function() x_inverse
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## cacheSolve function looks up whether the inverse of the actual matrix
## argument is cached. If it does, it returns the cached inverse matrix.
## If it does not, it calls get_inverse function in makeCacheMatrix
## compute the inverse and return it.

cacheSolve <- function(x, ...){
    x_inverse <- x$get_inverse()
    if(!is.null(x_inverse)){
        message("getting cached inverse matrix")
        return(x_inverse)
    }
    aMatrix <- x$get()
    x_inverse <- solve(aMatrix)
    x$set_inverse(x_inverse)
    x_inverse
}

