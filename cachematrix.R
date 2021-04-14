## These two functions helps find an inverse of a matrix
## by caching it.

## create a special "matrix" which contains function to 
## set/get the matrix and set/get the inverse of the matrix
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

## Function to find the inverse of a special "matrix" created with the above function.
## If the matrix already had an inverse matrix, return that result instead of recalculate.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data,...)
   x$setinverse(inv)
   inv
}
