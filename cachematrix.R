## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function that can make a matrix, get the matrix, make an inverse and put it in the cache

makeCacheMatrix <- function( m = matrix()) {
    #set cache initially to NULL
    cache = NULL
    
    #make matrix
    setmatrix <- function(y) {
        m <<- y
        cache <<- NULL
    }
    
    #get the matrix  
    getmatrix <- function() m
    
    #make inverse of matrix and cache it
    setinversematrix <- solve(m)
    cache <<- setinversematrix
    
    #get inverse of matrix from cache
    getinverse <- function() cache
  
    #put created functions into working environment  
    list(setmatrix = setmatrix, getmatrix=getmatrix,
         setinversematrix=setinversematrix,
         getinverse=getinverse)
  
}

## a function that can check if there is a inverse matrix in the cache and if so use it, if not
## make a new inverse matrix and put in the cache

cacheSolve <- function(m, ...) {
  #cache <- m$getinverse()
  
  if (!is.null(cache)) {
    print("getting matrix from cache")
    return(cache)
    
  }
  newmatrix <- m$getmatrix()    
  
  if (is.null(cache)) {
    print("calculating inverse")
    
    nm_inverse <- solve(newmatrix)
    cache <- nm_inverse
    
  put_in = {
    
    cache <<- nm_inverse
  }
    
    return(cache)
  }
  
} 
  
