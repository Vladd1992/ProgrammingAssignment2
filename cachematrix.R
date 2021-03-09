## This function creates a matrix that can inverse itself
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##cacheSolve checks if the inverse for the matrix has already been
# calculated if so then it returns it from the cache
# else it calculates the inverse using the solve function
# and sets it in the cache using setinv function
cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverted matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}