## This function keeps a copy of the matrix in the cache
makeCacheMatrix <- function(x) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the matrix
## created by makeCacheMatrix and stores the inverse of the
## matrix in the cache. During a call to calculate the inverse
## of a matrix, it checks if the inverse is in the cache.
## If the inverse is present, it uses the value from the cache.
## Otherwise, it does the inverse calculation.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

## The following is used to test the functionality of cachematrix.R
## Create Test Matrix
testmatrix <- matrix(1:4, 2, 2)

## Cache the Test Matrix
testcache <- makeCacheMatrix(testmatrix)

## Find the inverse and put the result in cache
testinverse <- cacheSolve(testcache)

## Printing Inverse
testinverse