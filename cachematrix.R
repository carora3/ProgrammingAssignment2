## This is my solution to Programming Assignment 2
## This will caache the values of a matrix and its inverse

##This function prepare the matrix that can be cached with its inverse
makeCacheMatrix <- function(m = matrix()){
  #t is inverse
  t <- NULL
  #set the matrix
  #double arrowed assignment is used to ensure assignment in parent function environment
  set <- function(y){
    m <<- y
    t <<- NULL
  }
  #get the matrix
  get <- function() m
  ##get and set the inverse
  setInverse <- function(inverse) t <<- inverse
  getInverse <- function() t
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse) 
}


## This function computes the inverse of a matrix, it assumes that the matrix is inversable
## The input is an object of makeCacheMatrix function

cacheSolve <- function(m, ...){
  t <- m$getInverse()
  ##Inverse already exists
  if(!is.null(t)){
      message("getting cached data")
      return(t)
  }
  data <- m$get()
    ##Compute inverse and return
  t <- solve(data, ...)
  m$setInverse(t)
  ##return inverse t
  t
}
