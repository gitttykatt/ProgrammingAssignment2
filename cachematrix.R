## The following function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() 
    list(set = set,        # gives the name 'set' to the set() function defined above
         get = get,        # gives the name 'get' to the get() function defined above
         setinv = setinv,  # gives the name 'setinv' to the setinv() function defined above
         getinv = getinv)  # gives the name 'getinv' to the getinv() function defined above
}


## The following function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
