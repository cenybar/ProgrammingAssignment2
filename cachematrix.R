## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##  This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the 
## inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the 
## inverse from the cache.
## Computing the inverse of a square matrix can be done with the `solve` function in R. For example, if `X` is a 
## square invertible matrix, then `solve(X)` returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Checking the code

m <- matrix(rnorm(16), 4, 4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

solve(m)
