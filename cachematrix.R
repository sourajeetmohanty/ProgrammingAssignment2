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

#testing of code
# x <- matrix(c(1, 2, 4, 6, 8, 9, 5, 8, 2), 3, 3)
# m <- makeCacheMatrix(x)
# m$get()
#      [,1] [,2] [,3]
#[1,]    1    6    5
#[2,]    2    8    8
#[3,]    4    9    2

cacheSolve(m)
#          [,1]       [,2]        [,3]
# [1,]  -1.3333333  0.7857143  0.19047619
# [2,]   0.6666667 -0.4285714  0.04761905
# [3,]  -0.3333333  0.3571429 -0.09523810
