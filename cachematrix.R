## Pair of functions to lazily calculate inverse of matrix when required
## and cache result

## Takes a matrix as the parameter
## Returns a wrapper that holds and matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newinverse) inverse <<- newinverse
  getinverse <- function() inverse
  
  
  
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
  
}

## Takes a matrix wrapper as the parameter
## Returns the inverse of a matrix held in a wrapper, using cached result if available
## otherwise calculating and caching before returning
cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  } else {
    data <- x$get()
    message("calculated inverse")
    i <- solve(data)
    x$setinverse(i)
    return(i)
  }
}