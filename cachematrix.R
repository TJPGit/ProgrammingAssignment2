## Programming Assignment 2: Caching the Inverse of a Matrix
## The pair of functions below allow the inverse of a matrix to be cached
## and retrieved (similarly to the "Caching the Mean of a Vector" example.

## makeCacheMatrix: this function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by
## makeCacheMatrix - if the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}



# ILLUSTRATIVE EXAMPLE (TO PROVE IT WORKS) IS COMMENTED OUT BELOW
# set.seed(42)
# mm <- matrix(rnorm(25), nrow=5, ncol=5)
# cc <- makeCacheMatrix(mm)
# cacheSolve(cc)  # first time - calculates and stores matrix inverse
# cacheSolve(cc)  # subsequent times - loads cached matrix inverse
# mm %*% cacheSolve(cc) # this returns the identity matrix (to within rounding error)

