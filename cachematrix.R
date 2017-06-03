## Programming Assignment #2 (Course 2, R programming, DataScience coursera.org)
## The purpose of these functions is to cache the results of a slow
## calculation. In this case it is for Matrix Inversion.
## The first function creates a faux matrix object that caches its own inverse.
## The second function calculates the inverse of the faux matrix object from
## from the first function - if its already cached it uses the cached value 
## instead of recalculating (to save time!)
##
## Code adapted from the https://github.com/rdpeng/ProgrammingAssignment2 
## sample code for caching Vectors by Roger D. Peng, last updated 2014
##
## Assumptions: the given (square) matrix is always invertible
##
## Function: makeCacheMatrix
## leverage the scoping in R to store values outside your environment
## This function sets a matrix (stores it for future reference) and nulls out the holder for the inversion
## This function gets the stored matrix
## This function inverts the matrix (via solve())
## This function gets the inverted matrix
## This function creates a list of each of the above functions


makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        setInv <- function(solve) matrixInverse <<- solve
        getInv <- function() matrixInverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Function: cacheSolve
## This function first checks to see if there's already an inverse defined for the given X
## if that's true, it retreives it
## If there's not already an inverse calculated, it sets the base matrix, then calculates 
## the inverse, and caches both, then returns the calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInv()
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data, ...)
        x$setInv(matrixInverse)
        matrixInverse
}
