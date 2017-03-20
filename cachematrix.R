## Programming Assignment 2: Caching the Inverse of a Matrix
## The pair of functions below allow the inverse of a matrix to be cached
## and retrieved (similarly to the "Caching the Mean of a Vector" example.

## makeCacheMatrix: this function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # initialise inv to NULL (not stored) 
        inv <- NULL
        # define setter function
        set <- function(y) {
                # store matrix
                x <<- y
                # set inv to NULL (as new matrix has been passed)
                inv <<- NULL
        }
        # define getter function
        get <- function() x
        # define setinverse function allowing the inverse to be stored
        setinverse <- function(inverse) inv <<- inverse
        # define getinverse function to retrive inverse
        getinverse <- function() inv
        # return a list to the user with set, get, setinverse and getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by
## makeCacheMatrix - if the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        # first run getinverse function
        inv <- x$getinverse()
        # if non-null, then there was a cached value - so return it
        if(!is.null(inv)) {
                message("getting cached matrix inverse")
                return(inv)
        }
        # otherwise the inverse needs to be recalculated and set
        # run the get function to retrive the matrix
        mat <- x$get()
        # then find the inverse using solve()
        inv <- solve(mat, ...)
        # then store a cache of the inverse by running setinverse
        x$setinverse(inv)
        # finally return the inverse to the user
        inv
}



### ILLUSTRATIVE EXAMPLE (TO PROVE IT WORKS) IS COMMENTED OUT BELOW...
# set.seed(42)
# mm <- matrix(rnorm(25), nrow=5, ncol=5)
# cc <- makeCacheMatrix(mm)
# cacheSolve(cc)  # first time - calculates and stores matrix inverse
# cacheSolve(cc)  # subsequent times - loads cached matrix inverse
# mm %*% cacheSolve(cc) # check that this indeed returns the identity matrix (to within rounding error)
# mmnew <- matrix(rnorm(9), nrow=3, ncol=3)  # define new matrix
# cc$set(mmnew)  # set up cc to use a different input matrix
# cacheSolve(cc)  # once again, the first time around we must calculate and store the new inverse
# cacheSolve(cc)  # subsequently, the inverse is retrieved from the cache
### end of example