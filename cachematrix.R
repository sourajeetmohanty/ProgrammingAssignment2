#the codes below are dedicated solely to find the inverse of matrix.



# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#the function below is used to find the inverse provided it has not been found yet, 
#and if it has been found then it returns it immidiately.


cacheSolve <- function(x, ...) {
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data.")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setinverse(inv)
  invr
}