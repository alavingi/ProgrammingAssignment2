## This files defines three functions
## makeCacheMatrix takes an invertible matrix as input and reurns a list of functions
## cacheSolve takes a special invertible matrix as a parameter and returns its inverse
## testCase is a test case for the above functions



## makeCacheMatrix takes an invertible matrix as input and reurns a list 
## of the following function:
## set - caches the input matrix
## get - retrieves the input matrix from the cache
## setinverse - caches the input inverse
## getinverse - retrieves the inverse from the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Save the input matrix to the cache
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Return the matrix from the cache
    get <- function() x
    # Save the inverse to the cache
    setinverse <- function(inverse) m <<- inverse
    # Return the inverse from the cache
    getinverse <- function() m
    # Return a list containing the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve takes a special invertible matrix as a parameter and retunrs its inverse
## If the inverse is found in the cache then it is returned
## Otherwise the inverse is computed, saved in the cache and returned

cacheSolve <- function(x, ...) {
    ## Check to see if the cache contains the inverse
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If the inverse is not found in the cache, compute it
    data <- x$get()
    m <- solve(data, ...)
    ## Save the inverse to the cache and return it
    x$setinverse(m)
    m
}

## Test case for the above functions
testCase <- function() {
    ## Create invertible matrix
    m <- matrix(c(1,0,5,2,1,6,3,4,0), nrow = 3, ncol = 3)
    ## Save to cache
    l <- makeCacheMatrix(m)
    l$set(m)
    # Compute inverse
    minverse <- solve(m)
    # Use cacheSolve to get the inverse
    msolve <- cacheSolve(l)
    print(minverse)
    print(msolve)
    ## Compare the above two matrices
    all.equal(minverse, msolve)        
}
