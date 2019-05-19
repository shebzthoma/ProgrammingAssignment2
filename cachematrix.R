## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix gets a matrix as its input, then creates a special
## matrix object that can then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

## This function creates a special "matrix" object that can cache its inverse
        matinvs <- NULL
        set <- function(y) {
                x <<- y
                matinvs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matinvs<<- inverse
        getinverse <- function() matinvs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## The function cacheSolve gets the output of previous function makeCacheMatrix
## and checks if the inverse has been claculated or not. If the inverse is calculated
## then it retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        matinvs <- x$getinverse()
        if(!is.null(matinvs)) {
                message("getting cached data")
                return(matinvs)
        }
        data <- x$get()
        matinvs <- solve(data, ...)
        x$setinverse(matinvs)
        matinvs
}
