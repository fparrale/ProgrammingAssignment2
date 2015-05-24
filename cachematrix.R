## Put comments here that give an overall description of what your
## functions that cache the inverse of a matrix


# Function that handles matrix type's objects in order to cache its inverse
makeCacheMatrix <- function(x=matrix()){
  ## Initializing variables
  matrixInv <- NULL
  
  ## Set the matrix
  set <- function(matrixOrig){
    matrix <<- matrixOrig
    matrixInv <<- NULL
  }
  
  ## Get the matrix
  get <- function(){
    ## returning matrix
    matrix
  }
  
  ## Set the inverse matrix
  setMatrixInv <- function(matrixInverse) {
    ## storing inverse 
    matrixInv <<- matrixInverse
  }
  
  ## Get the inverse matrix
  getMatrixInv <- function() {
    ## returns the inverse
    matrixInv
  }
  
  ## allow to know all methods 
  list(set = set, get = get,
       setMatrixInv = setMatrixInv,
       getMatrixInv = getMatrixInv)
}

## Function that handles the inverse of matrix's computation, if the inverse of 
## matrix have been calculated previously, return the previous result saved in cache.
cacheSolve <- function(x, ...) {
  # Get the inverse of a matrix that have been calculated previously
  matrixInv <- x$getMatrixInv()
  
  ## If have been calculated previously, will return its matrixInv result
  if(!is.null(matrixInv)) {
    message("returning inverse of matrix saved in cache")
    return(matrixInv)
  }
  ## Else, need to be calculated
  
  ## first, retrieve matrix from object 
  matrix <- x$get()
  
  ## then, get the matrix inverse by matrix multiplication method
  matrixInv <- solve(matrix) %*% matrix
  
  ## After that, save the inverse of matrix that have been calculated
  x$setMatrixInv(matrixInv)
  
  ## Finally, return the inverse matrix as result
  matrixInv 
}
