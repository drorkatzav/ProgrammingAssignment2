## This creates matrix object that can hold cached inverse for the matrix.
## The inverse is calculated using the cacheSolve function that returns the 
## cached inverse if calculated and calculate inverse otherwise.
## This was created as part of ProgrammingAssignment2 for R Programming.
## Created by Dror Katzav. Date: April 2015.

## This function creates an object that contains a matrix.
## The supported functions are get/set and getinverse/setinverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Return an object by listing the supported functions.
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(set_inverse) inverse <<- set_inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function operates on objects of type makeCacheMatrix.
## The function chcecks if an inverse is stored and calculate using solve otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
