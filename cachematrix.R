## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the 
## inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). 
## Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    Minverse <- NULL
    
    ## sets the value of the matrix
    set <- function(f) {
        x <<- f
        Minverse <<- NULL
    }
    
    ## gets the value of the matrix
    get <- function() x
    
    ## sets the value of the inverse matrix
    setinverse <- function(inverse) Minverse <<- inverse
    
    ## gets the value of the inverse matrix
    getinverse <- function() Minverse
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    Minverse <- x$getinverse()
    if(!is.null(Minverse)) {
        message("getting cached data.")
        return(Minverse)
    }
    
    data <- x$get()
    Minverse <- solve(data)
    x$setinverse(Minverse)
    
    ## Return a matrix that is the inverse of 'x'
    Minverse
    
}
