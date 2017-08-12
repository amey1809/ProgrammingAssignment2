## Put comments here that give an overall description of what your
## functions do

## Following function makeCacheMatrix used for set value of matrix, get value of matrix
## set value of matrix inverse, get valueof matrix inverse

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


## cacheSolve function calculates the matrix inverse,for the input matrix  supplied 
## from makeCacheMatrix function and checks if we have already cached the  the inverse 
## matrix for this matrix if inverse is already computed it returns cached value 
## for matrix inverse; otherwise it computes the matrix inverse and returns value.
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


