#  makeCacheMatrix: this function creates a special "Matrix" object that can cache its inverse.
#  1. set the value of the Matrix
#  2. get the value of the Matrix
#  3. set the value of inverse of the matrix
#  4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#  cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), 
#  then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}