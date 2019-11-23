## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inverse_mat <- NULL
  set <- function(y){
    x <<- y
    inverse_mat <<- NULL
  }
  get <- function() x
  setInverse_mat <- function(solveMatrix) inverse_mat <<- solveMatrix
  getInverse_mat <- function() inverse_mat
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  inverse_mat <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inverse_mat)
  }
  data <- x$get()
  inverse_mat <- solve(data)
  x$setInverse(inv)
  inverse_mat        
  
}
