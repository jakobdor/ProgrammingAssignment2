## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(matrix ) {
    mat <<- matrix
    inv <<- NULL
  }
  get <- function() {
    mat
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  mat <- x$getInverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data) %*% data
  x$setInverse(mat)
  mat
}

##Let's see if this works.
x <- rbind(c(1, 2, 3), c(0.5, 6, 9.9))
mat <- makeCacheMatrix(x)
cacheSolve(mat)
cacheSolve(mat)

##Error in solve.default(data) : 'a' (2 x 3) must be square
##-> let's try again with a square matrix

x <- rbind(c(1, 2, 3), c(0.5, 6, 9.9), c(0.5, 0.1, 4))
mat <- makeCacheMatrix(x)
cacheSolve(mat)
cacheSolve(mat)

##Perfect!