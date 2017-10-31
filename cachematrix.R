#Matrix inversion is usually a costly computation 
#and there may be some benefit to caching the inverse 
#of a matrix rather than compute it repeatedly 
#(there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.


#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-NULL
  getinverse<-function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# This function calculates the  inverse of the special "matrix" created by 
# above function. However, if the inverse of special "matrix" has already 
# been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

