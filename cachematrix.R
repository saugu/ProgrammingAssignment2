## This solution shows how caching can be used for repetitive functions. What we do first
## is calculating the inverse and storing it in a different environment, if we run makeCacheMAtrix
## function alone, it would return a list of functions. catcheSolve function checks whether
## the solution is already calculated and returns it or calculates otherwise. 

## A short comment describing this function - this function captures the functions needed
## for the calculation of inverse for a matrix 'x'. We first set a 'placeholder'for inverse 
## as 'inverse', then we create four functions: 
                        # 1. set = set matrix x to a new matrix y.
                        # 2. get = return matrix x
                        # 3. setInverse = set the inverse function by using function solve()
                        # 4. getInverse = return the inverse 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL 
        set <- function(y) {
            x <<- y
            inverse <<- NULL 
        }

        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }

## A short comment describing this function - this function first looks whether
## there is a value for Inverse of the matrix x stored already (given the function above).  
## If the value is already stored (or if 'inverse' is not equal to 0), getInverse returns it. 
## If the value is not stored, the function calculates the inverse for matrix x.  

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        ## Return a matrix that is the inverse of 'x'
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
     
}
