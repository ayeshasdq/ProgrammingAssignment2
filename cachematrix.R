# makeCacheMatrix
makeCacheMatrix <- function(x = numeric()) {
  
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# Inverse of a "special" matrix is created with ###makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cache value
  inverse <- y$getInverse()
  # if a cache value exists, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise getting matrix
  
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # returning inverse
  inverse
}
