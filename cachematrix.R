## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) matrixInv <<- inverse
  getMatrixInverse <- function() matrixInv
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
  
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixInv <- x$getMatrixInverse()
  if (!is.null(matrixInv)) {
    message("getting cached data")
    return(matrixInv)
  }
  mat <- x$get()
  matrixInv <- solve(mat, ...)
  x$setMatrixInverse(matrixInv)
  matrixInv
  
}


