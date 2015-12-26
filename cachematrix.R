## Summary: makeCacheMatrix creates a data structure that enables 
## cacheSolve to store the inverse of a matrix the first time it
## is computed (and skip the computation on future calls).

## For example,
##
## GIVEN:
## m <- matrix(c(4, 2, 7, 6), 2, 2)
## m
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
##
## WHEN:
## c <- makeCacheMatrix(m)
## s <- cacheSolve(c)
##
## THEN:
## s
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4


## Creates a data structure that will contain both the specified
## matrix and a place for cacheSolve to cache the inverse of the
## specified matrix.
##
## @param x A square matrix
## @return A vector of functions to get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Returns the inverse of the matrix that is contained in 'x',
## provided x was created with makeCacheMatrix.
##
## @param x A matrix cache created with makeCacheMatrix
## @return A the inverse of the matrix cached in x
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
