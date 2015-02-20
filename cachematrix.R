## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## If a matrix is not provided, it is initilized to an empty matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL ## initialize local m to NULL
  
  set <- function(y) {  ## create the set function
    x <<- y             ## save the value of y, passed into the function to x
    m <<- NULL          ## 'global' m is set to NULL
  }
  
  get <- function() x   ## return the value set, by the set function
  
  setinverse <- function (inverse)  m <<- inverse ## pass the inverse and set it to 'global' m
  
  getinverse <-  function() m  ## return the cached global 'm'
  
    
  ## Return the list of functions (or accessor methods) for this function
  list (set = set, get= get, setinverse = setinverse, getinverse=getinverse)
  
}


## This function returns the matrix that is the inverse of 'x', and caches it, if the 
## inverse has been calculated it returns the cached value

cacheSolve <- function(x, ...) {
    
  m <- x$getinverse ()     ## call getinverse from makeCacheMatrix
  if(!is.null(m)) {  ## if a previous value is cached then
    message("getting cached data")  ##print a message 
    return(m)   ##Returns the cached value of m (the inverse matrix)
  }
  data <- x$get()  ## if there is no previous value, then get the data from the get function
  m <- solve(data, ...)  ## take the inverse using the 'solve' function
  x$setinverse(m)  ##cache the inverse using setinverse
  m                ##return 'm' (the resulting inverse matrix)
  
}
