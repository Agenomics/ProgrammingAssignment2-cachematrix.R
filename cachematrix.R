## This is the process to create a square invertible matrix efficiently
##to run the full process type:  makeCacheMatrix(matrix(c(nx,...),r,c))
##where nx=your matrix numbers (as many as you wish that form a matrix)
##where r= # of rows, c=# of columns

##first process creates the matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##this second function is intended to improve processing efficiency
##is does this by checking if variable already exists in cache
##if it does (not null) it will not recalculate the inverse matrix(solve)

  cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
            return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
  }
  