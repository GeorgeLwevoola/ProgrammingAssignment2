## This function creates a special "matrix" object that can cache its inverse.
## The matrix must be square

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(nrow=z,ncol=z)) {
  matrix_inverse <- NULL
  set <- function(cacheM) {
    x <<- cacheM
    matrix_inverse <<- NULL
  }
  get <- function() x
  setmatrix_inverse <- function(solve) matrix_inverse <<- solve
  getmatrix_inverse <- function() matrix_inverse
  list(set = set, get = get,
       setmatrix_inverse = setmatrix_inverse,
       getmatrix_inverse = getmatrix_inverse)
  

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' using the solve function
  ## check for cached matrix
  matrix_inverse <- x$getmatrix_inverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached matrix")
    return(matrix_inverse)
  }
  ## if cached matrix doesnt exist
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setmatrix_inverse(matrix_inverse)
  matrix_inverse

}
