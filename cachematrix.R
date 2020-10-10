##Christian Fernando López Orozco
## This function will create a matrix and put into a cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This matrix will calculate the inverse, but if the inverse was already
## calculated before, then it will avoid the calculation and return the cache value

cacheSolve <- function(x, ...) { m <- x$getinverse() 
if(!is.null(m)) {
  message("getting cached data")
  return(m)
}
data <- x$get()
m <- solve(data, ...)
x$setinverse(m)
m
}
## Return a matrix that is the inverse of 'x'


