## Assignment: Caching the Inverse of a Matrixless 

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:
    
#-1 makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#-2 cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.



makeCacheMatrix <- function(x = matrix()) {

 inv <- NULL
 set <- function(y) {                            # -> set the value of the matrix
     x <<- y
     inv <<- NULL
 }
 get <- function() x                             # -> get the value of the matrix
 setinv <- function(inverse) inv <<- inverse     # -> set the value of inverse of the matrix
 getinv <- function() inv                        # -> get the value of inverse of the matrix
 list(set=set, get=get, setinv=setinv, getinv=getinv)     

}


## Write a short comment describing this function
# This function returns the inverse of the matrix :

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if(!is.null(inv)) {                         # -> the inverse has already been computed
       return(inv)                              # -> if this is the case, retrieve data from cache
    }
    data <- x$get()                             # -> if not
    inv <- solve(data)                          # -> then compute the inverse
    x$setinv(inv)                               # -> and sets the value 
    inv
        ## Return a matrix that is the inverse of 'x'
}
