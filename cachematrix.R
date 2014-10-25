## Function returning the inverse matrix of a nonsingular matrix

## makeCacheMatrix returns a function list that would be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inverse object 
  m <- NULL
  
  ## check is the matrix already exists
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # return the object from higher level scope
  get <- function(){
    x
  }
  
  ## set the inverse in higher level
  setInverse <- function(inverse){
    m <<- inverse
  }
  
  ## get the inverse from higher level
  getInverse <- function(){
    m
  } 
  
  ## combine the four functions into a list variable as an output
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function is for getting the inverse matric from the cache if it exist, or create one
## and store the inverse matrix in it if cache does not exist.

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
