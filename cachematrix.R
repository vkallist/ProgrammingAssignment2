## We implement two functions. The first, makeCacheMatrix, creates a special 
## matrix object which caches its inverse.
## The second function, cacheSolve, demonstrates how to use the first function 
## in order to implement a fast matrix inversion. It computes the inverse 
## of a given matrix only once, and then whenever the inverse is requested
## again, returns the cached value

## makeCacheMtrix returns a set of functions to create and access a matrix, 
## as well as to set and retrieve its inverse. Also it creates a variable inv
## which caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<-y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(i) inv <<- i
    
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve gets as parameter the special "matrix" created by the above function
## It checks whether the inverse of the matrix has already been computed, in which
## case the cached value is returned. Otherwise, it computes the inverse of the
## matrix, stores it in the cache variable of the special "matrix" and returns
## the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getinv()
    ## first case: inverse has already been computed in the past
    if (!is.null(inv)) {
        return(inv)
    }
    ## second case: inverse has not been computed yet
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
