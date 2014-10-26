## Function returning the inverse matrix of a nonsingular matrix

## makeCacheMatrix returns a function list that will be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize inverse object 
  m <- NULL
  
  ## Methord to set the matrics
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # Method to return the matrix from higher level scope
  get <- function(){
    x
  }
  
  ## Method to set the inverse matrix in higher level
  setInverse <- function(inverse){
    m <<- inverse
  }
  
  ## Method to get the inverse matrix from higher level
  getInverse <- function(){
    m
  } 
  
  ## Combine the four functions into a list variable as an output
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function is for getting the inverse matric from the cache if it exist, or creating one
## and storing the inverse matrix in it if cache does not exist.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()

  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from object
  data <- x$get()
  
  ## Calculate the inverse of matrix
  m <- solve(data,...)
    
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
