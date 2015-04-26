## makeCacheMatrix allows you to create a matrix object with a list of functions
## it can apply to itself. The cacheSolve function can be used on the object
## created by makeCacheMatrix to either retrieve a cached inverse matrix, or
## calculate the inverse matrix and use the object's setinverse function to 
## cache it for use later.


## This function creates a matrix object with a list of functions that can be
## applied to it externally. It can store and retrieve an regular/inverse versions 
## of itself using the set/get[inverse] attributes.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function is used in combination with makeCacheMatrix to retrieve a cached
## inverse matrix or calculate the inverse of the given matrix and store the cache
## into the makeCacheMatrix inv variable.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}