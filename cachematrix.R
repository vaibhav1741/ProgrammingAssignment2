
#Creates a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inverse_mat <- NULL
  
  ## A method to set the matrix
  set <- function(y){
    x <<- y
    inverse_mat <<- NULL
  }
  
  ## A method to get the matrix
  get <- function() x
  
  ## A method that set the inverse of the matrix
  setInverse_mat <- function(solveMatrix) inverse_mat <<- solveMatrix
  
  ## A method to get the inverse of the matrix
  getInverse_mat <- function() inverse_mat
  
  ## Return  a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"above. If the inverse has already been
## calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse_mat <- x$getInverse()
  
  ## Returns the inverse if its already set  
  if(!is.null(inv)){
    message("getting cached data")
    return(inverse_mat)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inverse_mat <- solve(data)
  
  ## Set the inverse to the object
  x$setInverse(inv)
  
  ## Return the matrix
  inverse_mat        
  
}
