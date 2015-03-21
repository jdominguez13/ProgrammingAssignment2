# makeCacheMatrix creates a list containing a function that will
  # 1. Set the value of the Matrix
  # 2. Get the value of the Matrix
  # 3. Set the inverse of the Matrix
  # 4. Get the inverse of the Matrix
makeCacheMatrix <- function (x = matrix()) {
    i <- NULL
    # 1. Set Matrix
    set <- function (y) {
           x <<- y
           i <<- NULL
    }
    # 2. Get Matrix
    get <- function () x
           # 3. Set Inverse
           setinv <- function (inv) i <<- inv
           # 4. Get Inverse
           getinv <- function () i
           list(set=set, get=get, setinv=setinv, getinv=getinv)
    }
# This function will check if the inverse of the matrix has already been computed.
# If it has been computed, it will show the result and skip the computation.
# If not, it will set the value of the inverse matrix in the cache by using the
# the setinverse function specified above.
cacheSolve <- function(x, ...) {
  # Check for the Inverse Matrix
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  # Compute de Inverse Matrix
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
