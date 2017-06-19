## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL ## set the inverse to null
  
  set_matrix <- function(y) {  ## set the matrix
    x <<- y ## make the value of matrix y
    inverse <<- NULL ## make the inverse null
    
  }
  
  get_matrix <- function() x ## get the value of the matrix
  
  set_inverse <- function(solve_from_cacheSolve) {  ## make the inverse from cacheSolve the inverse
    inverse <<- solve_from_cacheSolve
  }
  
  get_inverse <- function() inverse ## return the inverse
  
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
  
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$get_inverse() ## get the inverse from the previous makeCacheMatrix
  
  ## if the inverse already exists, send the message, along with the mean
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## if inverse doesn't exist...
  
  data <- x$get_matrix()  ## get the matrix in x
  i <- solve(data, ... ) ## calculate the inverse and set to i
  x$set_inverse(i) ## run the set_inverse function
  i ## return the inverse
  
}


## Test scripts

## a<-makeCacheMatrix()
## a$set_matrix(matrix(rnorm(16),4,4))
## a$get_inverse()
## cacheSolve(a)