## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This two functions allow for the creationg and caching of the inverse of a 
## matrix

## The first function, makeCacheMarix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinvert <- function(invert_matrix) inv_matrix <<- invert_matrix
  getinvert <- function() inv_matrix

  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert) 
}


## Write a short comment describing this function
## The following function checks in the inverse of the special matrix exist and
## if not calculates and set the inverse of the special "matrix".
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <-x$getinvert()
  if(!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <-solve(data)
  x$setinvert(inv_matrix)
  inv_matrix
}

## Test run:
m = matrix(c(2,3,6,5,8,12,3,6,11), nrow = 3, ncol = 3);
c = makeCacheMatrix(m)

c$get()
##      [,1] [,2] [,3]
##[1,]    2    5    3
##[2,]    3    8    6
##[3,]    6   12   11
cacheSolve(c)
##      [,1]       [,2]        [,3]
##[1,]  1.4545455 -1.7272727  0.54545455
##[2,]  0.2727273  0.3636364 -0.27272727
##[3,] -1.0909091  0.5454545  0.09090909
cacheSolve(c)
## getting cached inverse matrix
##      [,1]       [,2]        [,3]
##[1,]  1.4545455 -1.7272727  0.54545455
##[2,]  0.2727273  0.3636364 -0.27272727
##[3,] -1.0909091  0.5454545  0.09090909