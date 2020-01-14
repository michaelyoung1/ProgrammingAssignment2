## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function. 
## This function get the values of an input matrix and it's stored inverse value (if it exists), otherwise it assigns a null value 
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    #x<-y
    inv <<- NULL
  }
  get <- function() { x }
  setinv <- function(invsr) {inv <<- invsr}
  getinv <- function() {inv}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function.  This function retrieves the stored inverse value of a matrix (if it exists). 
## Otherwise, it computers the inverse value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  #invsr <- solve(data, ...)
  i<-  solve(data) 
  x$setinv(i)
  i
}


#
