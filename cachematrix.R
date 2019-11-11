## In this project cache is used to get the inverse of a matrix in a fast way 
## if it has already been calculated. Two function are defined:

## First function, makeCacheMatrix, creates a special matrix  
## object that can cache its inverse.   

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix
  inv_m <- NULL
  
  ## Set the value of the matrix
  set <- function(y) 
    x <<- y
    inv_m <<- NULL
  
  get <- function()  ## Get the value of the matrix
    x ## Display the Matrix
  setInverse <- function(inverse) inv_m <<- inverse  ## Set the inverse of the matrix
  getInverse <- function()  ## Get the inverse of the matrix
    inv_m   ## Return the inverse matrix
  
  ## List of the used functions.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Second function, cacheSolve, compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Inverse of the matrix
  inv_m <- x$getInverse()
  
  ## Return the inverse if already set
  if( !is.null(inv_m) ) {
    message("getting cached data")
    return(inv_m)
  }
  
  data <- x$get()
  
  ## Calculate the inverse
  inv_m <- solve(data,...) 
  
  ## Set the inverse
  x$setInverse(inv_m)
  
  ## Display the matrix
  inv_m
}
