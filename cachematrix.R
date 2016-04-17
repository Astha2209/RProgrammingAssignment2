## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly

## A pair of functions are written below that are used to 
## create a special "matrix" object that can cache its inverse

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
  }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
                        setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}


## Sample Run
## > x <- matrix(6:9, 2, 2)
## > a = makeCacheMatrix(x)
## > a$get()
##      [,1] [,2]
## [1,]    6    8
## [2,]    7    9
##
## > a$getInverse()
## NULL
## 
## No cache in the first run
##
## > cacheSolve(a)
##      [,1] [,2]
## [1,] -4.5    4
## [2,]  3.5   -3
##
## In the second run, it retrieves from the cache
##
## > cacheSolve(a)
## Getting cached data
##      [,1] [,2]
## [1,] -4.5    4
## [2,]  3.5   -3
