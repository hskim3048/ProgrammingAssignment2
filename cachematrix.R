### Assignment: Caching the Inverse of a Matrix

## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The benefit of caching the inverse of a matrix lies in avoiding repeated
## costly computation of matrix inversion.

## The function called `makeCacheMatrix` can be used to create a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
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

## The function `cacheSolve` computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix' and retrieves the inverse from the cache
## if it has already been calculated (an the matrix has not changed).

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
