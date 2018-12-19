# This function cache the Inverse of a Matrix

makeCacheMatrix <- function(x=matrix()) {
  Inverse <- NULL
  
  # set the matrix
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set the inverse of matrix
  setInverse <- function(x)  {
    Inverse <<- solve(x)
  }
  
  # get the inverse of marix
  getInverse <- function() Inverse
  
  
  list(set = set, get = get,
       setInvrs= setInverse,
       getInvrs= getInverse)
  
}

cacheSolve <- function(x=matrix(), ...) {
  Inverse <- x$getInvrs()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data,...)
  x$setInvrs(Inverse)
  return(Inverse)
}
