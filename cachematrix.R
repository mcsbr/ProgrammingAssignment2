## Created by mcsbr on 09/26/2015
##
## BEGIN OF Functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix caches matrix and its inverse
## cacheSolve calculates inverse of matrix cached in makeCacheMatrix

## Function makeCacheMatrix is a special function that
##  contains a list of the following functions: 
##   1) set the value of the matrix
##   2) get the value of the matrix
##   3) set the value of the inverse of the matrix
##   4) get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function cacheSolve calculates the inverse of the matrix
## created in the function makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## first checks if the inverse already exists
  if( !is.null(m) ) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return (m)
}

## END OF Functions: makeCacheMatrix and cacheSolve