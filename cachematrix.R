
## Written by Subanatarajan Subbiah

## The function "makeCacheMatrix" creates a special "matrix" which is really a list 
# 1) set the elements of the matrix
# 2) get the elements of the matrix
# 3) set the elements of the inverse of the matrix
# 4) get the elements of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solveM) inverseM <<- solveM
  getInverse <- function() inverseM
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cachesolve <- function(x, ...)
{
  inverseM <- x$getInverse()
  if(!is.null(inverseM)) {
    message("getting cached data")
    return(inverseM)
  }
  data <- x$getMatrix()
  inverseM <- solve(data, ...)
  x$setInverse(inverseM)
  inverseM
}





