makeCacheMatrix <- function(x = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Compute and cache the inverse of a matrix
#' @param x the result of a previous makeCacheMatrix call
#' @param ... additional arguments to pass to solve function
#' examples
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
