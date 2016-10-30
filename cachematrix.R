# Relies on package "matrixcalc"" to check for matrix type as input
#
# For this assignment, assume that the matrix supplied is always invertible.
# Ref: https://en.wikipedia.org/wiki/Invertible_matrix
#
# Utilizes <<- operator to assign a value to an object in an environment that is different from the current environment

if(!require("matrixcalc")){
  install.packages("matrixcalc")
  library(matrixcalc)
}

# "makeCacheMatrix" function returns a list of function calls
#
# set the matrix
# get the matrix
# set inverse of the matrix
# get inverse of the matrix
	
makeCacheMatrix <- function(x = matrix()) {
  if (is.matrix(x) && is.square.matrix(x)) {
    inv = NULL

    set <- function(y) {
      x <<- y
      inv <<- NULL
    }

    get <- function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
 
    # return list
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  } else {
    # Abort
    stop("Accepts only square matrix!", call. = FALSE)  
  } 
}

# Matrix inversion is costly computation...caching the inverse of a matrix rather than computing it repeatedly
#
# "cacheSolve" function first checks to see if the inverse of matrix has already been calculated. 
# If so, it gets the inverse of matrix from the cache and skips the computation. 
# Otherwise, it inverses the matrix using "solve" function and returns inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("\n...getting cached data...\n")
    return(inv)
  }
  
  inv = solve(x$get(), ...)
  
  x$setinv(inv)
  return(inv)
}

### Test using
### ============Good=======================
# m <- matrix(rnorm(100), nrow = 10)
# mat <- makeCacheMatrix(m)
# cacheSolve(mat)
# cacheSolve(mat)
### ============Bad============================
# m <- matrix(rnorm(100), nrow = 5)
# mat <- makeCacheMatrix(m)
### =============================================
