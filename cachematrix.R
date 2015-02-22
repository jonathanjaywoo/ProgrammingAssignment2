# This pair of functions basically creates a cached matrix object
# that solves the inverse of the matrix,
# but the cool thing is that it only calculates it once.

# In this function, it creates a cache matrix.
makeCacheMatrix <- function(x = matrix()) 
{
  inv = NULL
  set <- function(y) # The set function is what sets the matrix
  {
      x <- y
      inv <- NULL
  }
  get <- function() x # The get function is where you get your matrix.
  
  setInverse <- function(inverse) inv <- inverse # sees if there is already is set.
  getInverse <- function() inv # if there is an inverse, then you get the inverse. Otherwise, its null.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  
}

# In this function, it returns the inverse of the above function.
cacheSolve <- function(x, ...)
{
  inv <- x$getInverse()
  if(!is.null(inv))
  {
      return(inv)
  }
  type <- x$get()
  inv <- solve(type,...)
  x$setInverse(inv)
  return(inv)
  
}

# Examples of usage:
# CM <- matrix(c(3, 4, 2, 6), 2, 2)
# cache <- makeCacheMatrix(CM)
# cacheSolve(cache)