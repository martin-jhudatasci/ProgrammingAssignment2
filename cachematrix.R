# cachematrix.R
# Functions to provide a wrapper around a matrix object, which can cache its inverse,
# and calculate the inverse (used the cached value if available)

# Creates a matrix wrapper object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # set default value to NULL, before first calculation
  
  # set/get functions for the matrix itself
  set <- function(y) {
    x <<- y
    inverse <- NULL
  }
  get <- function() {
    return(x)
  }
  
  # set/get functions for the inverse
  set.inverse.value <- function(inv) {
    inverse <<- inv
  }
  get.inverse.value <- function() {
    return(inverse)
  }
  
  return(list(set = set, 
              get = get,
              set.inverse.value = set.inverse.value,
              get.inverse.value = get.inverse.value))
}


# Returns solve(x), using cached value if available
cacheSolve <- function(x, ...) {
  inv <- x$get.inverse.value()
  
  # if result already cached
  if (!is.null(inv)) {
    message("returning cached result")
    return(inv)
  }
  
  # otherwise, calculate 
  m <- x$get()
  m.inv <- solve(m)
  x$set.inverse.value(m.inv)
  return(m.inv)
}
