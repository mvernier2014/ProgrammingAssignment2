# Programming assignment #3
#
# Matt vernier
#
# This first function will take the target matrix (x) and put it into cached
# memory along with its inverse to be  retrieved later and compared
#
# The example given by R.D. Peng was used as a template for my code

makeCacheMatrix <- function(x = matrix()) {
  # Create Set function to put target matrix in cached memory
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Create Get function to put the inverse matrix in cached memory
  
  get <-function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

# This second function compares the current matrix to the cached version,
# and if the two are identical, display the cached inverse
#
# The example given by R.D. Peng was used as a template for my code

cacheSolve <- function(x, ...) {
  
  # If the current matrix is null, retrieve the cached version
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message ("Getting cached data")
    return(m)
  }
  
  # If the current matrix is not null, invert the matrix and display the
  # solution
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}