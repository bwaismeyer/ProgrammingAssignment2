## A pair of functions for caching a matrix
## and finding and caching its inverse.

## This function creates an object which contains a matrix, 
## the inverse of that matrix (once known), 
## and functions for getting and setting the matrix and inverse.

makeCacheMatrix <- function(cached_matrix = matrix()) {
  # the cached matrix was defined in the function args above
  # here we give an initial definition of the matrix inverse 
  # (set to NULL because we don't know the inverse yet)
  cached_inverse <- NULL
  
  # define functions for interacting with the cached matrix
  set_matrix <- function(matrix_data) {
    cached_matrix  <<- matrix_data
    cached_inverse <<- NULL
  }
  get_matrix <- function() cached_matrix
  
  # define functions for interacting with the cached inverse
  set_inverse <- function(matrix_inverse) {
    cached_inverse <<- matrix_inverse
  }
  get_inverse <- function() cached_inverse
  
  # return list of functions for interacting with our special matrix object
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function interacts with our special matrix object.
## It checks to see if the inverse has been calculated yet.
## If not, it works on the cached matrix and finds and caches its inverse.

cacheSolve <- function(matrix_object, ...) {
  # retrieve the current cached inverse from the object
  matrix_inverse <- matrix_object$get_inverse()
  
  # if the current inverse is non-null...
  # assume it is the calculated inverse and return it
  if(!is.null(matrix_inverse)) {
    message("Retrieving cached inverse...")
    return(matrix_inverse)
  }
  
  # if the current inverse is null...
  # find the correct inverse, set it, and return it
  cached_matrix <- matrix_object$get_matrix()
  matrix_inverse <- solve(cached_matrix, ...)
  matrix_object$set_inverse(matrix_inverse)
  
  matrix_inverse
}
