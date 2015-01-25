## makeCacheMatrix is a function that creates a special "matrix" object
## to set the values of each entry in the matrix, get the value of each 
## entry in the matrix, set the inverse of the matrix and finally get 
## the inverse of the matrix.  

## cacheSolve computes the inverse of the matrix and ensures that if 
## the inverse is computed before, it retrieves the data from the cache. 

## This function creates a special "matrix object" that can cache its inverse:

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
}
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix and ensures to retrieve the inverse from the cache if 
## the inverse has been calculated before. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) 
  m  
}
        

