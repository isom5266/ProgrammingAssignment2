##This assignment writes the functions 'makeCacheMatrix' and 'cacheSolve' which
###Cache the inverse of a matrix


#makeCacheMatrix creates a matrix that can cache...
##... its inverse for the input


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#cacheSolve calculates the inverse of the matrix from makeCacheMatrix
#It will first make sure that the inverse has been correctly calculated
##If so, it will get the inverse
###If not, it will calculate the inverse, and reset the inverse in the cache via the setinv function


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}