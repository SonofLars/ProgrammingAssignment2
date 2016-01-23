## This pair of functions allows a matrix and its computed inverse to be
## cached to save computing resources for the future

## makeCacheMatrix creates a list of 3 functions that allow us to:
## 1. Fetch the original input matrix
## 2. Set the cache with the inverse matrix
## 3. Fetch the stored inverse matrix to check if it exists

makeCacheMatrix <- function(x = matrix()) {
    CachedMatrix <- NULL

    get <- function() x
    setinverse <- function(solve) CachedMatrix <<- solve
    getinverse <- function() CachedMatrix
    
    list(get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## Takes the list created by makeCacheMatrix and
## 1. Grabs cached value
## 2. Checks if cached value is NULL, therefore nothing is truly cached
## 3. If cached inverse exists, returns a message stating the case and
##    returns the matrix
## 4. If value is NULL, computes inverse of originally input matrix and
##    returns that inverse matrix

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
    CachedMatrix <- x$getinverse()
    if(!is.null(CachedMatrix)) {
        message("getting cached data")
        return(CachedMatrix)
    }
    data <- x$get()
    CachedMatrix <- solve(data,...)
    x$setinverse(CachedMatrix)
    CachedMatrix
}