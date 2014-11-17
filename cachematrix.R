## Put comments here that give an overall description of what your
## functions do

## This function is a sort of wrapper of a matrix.
## It permits to save the inverse of that matrix once is calculated
## It is a list which contains 4 functions
## set: set the matrix (which is supposed to be invertible)
## get: get the matrix
## setinverse: set the inverse of the matrix
## getinverse: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    # When a new matrix is inserted the inverse must be setted to NULL
    ## Note that all the inner functions use the superassignment operator
    ## to store the variables'values to an higher environment
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  # It returns the list which contains the 4 functions mentioned above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This is a handler to get the inverse of the matrix stored in makeCacheMatrix.
## This function check if there is a cached version of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    # Checking the cache. If is not NULL returns it
    message("getting cached data")
    return(inv)
  }
  # If the cache is NULL must be computed (and stored in the cache)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
