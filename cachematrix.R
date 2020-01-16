## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function. 
## This function get the values of an input matrix and it's stored inverse value (if it exists), otherwise it assigns a null value 
## 

makeCacheMatrix <- function(x = matrix()) {
   #variable that will store the inverse of a matrix. 
  inv <- NULL
   # Set the input matrix. 
  set <- function(y) {
    x <<- y
    #x<-y
    inv <<- NULL
  }
    #Get the matrix
  get <- function() { x }
    #Setting the inverse of a matrix
  setinv <- function(invsr) {inv <<- invsr}
    #Getting the inverse
  getinv <- function() {inv}
    #Putting all functions together in a list. 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function.  This function retrieves the stored inverse value of a matrix (if it exists). 
## Otherwise, it computers the inverse value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
    #If the inverse already exists, get it from the cache.
  if(!is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
    # If the inverse does not exist,  calculate the inverse and store it in the cache and return the inverse. 
  data <- x$get()
  i<-  solve(data) 
  x$setinv(i)
  i
}



