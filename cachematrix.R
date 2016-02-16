
## Written by Subanatarajan Subbiah

# The function "makeCacheMatrix" creates a special "matrix" which is really a list 
# 1) set the elements of the matrix
# 2) get the elements of the matrix
# 3) set the elements of the inverse of the matrix
# 4) get the elements of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  # 
  inverseM <- NULL
  
  # setMatrix function: sets the elements of the matrix 
  setMatrix <- function(y = matrix()) 
  {
    x <<- y
    inverseM <<- NULL
  }
  
  
  # getMatrix function: gets the matrix elements
  getMatrix <- function() 
  {x}
  
  #setInverse function: sets the elements of the inverse matrix
  setInverse <- function(solveM) 
  {inverseM <<- solveM}
  
  #getInverse function: gets the elements of the inverse matrix
  getInverse <- function() 
  {inverseM}
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The function "cachesolve" calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function

cachesolve <- function(x, ...)
{
  inverseM <- x$getInverse()
  if(!is.null(inverseM)) {
    message("getting cached data")
    return(inverseM)
  }
  data <- x$getMatrix()
  
  # Use the solve() function to calculate the inverse of the matrix
  inverseM <- solve(data, ...)
  
  x$setInverse(inverseM)
  inverseM
}



# To check the above functions an example test script is given below
# matrixA <- makeCacheMatrix(matrix(sample(1:20,9), 3, 3))
# matrixA$getMatrix() # returns a 3x3 matrix
# matrixA$getInverse() #returns a NULL
# cachesolve(matrixA) # returns the inverse of the 3x3 matrix
# matrixA$getInverse() # returns the inverse of the 3x3 matrix
# matrixA$setMatrix((matrix(sample(1:20,16), 4, 4))) #Creates a new random 4x4 matrix and stores it in variable "matrixA"
# cachesolve(matrixA) # returns the inverse of the new 4x4 matrix
# matrixA$getInverse() # returns the inverse of the 4x4 matrix



