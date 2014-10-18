## Below are two functions created in order to be able to eliminate repetitive
## calculations of inverting a matrix. 


## makeCacheMatrix takes a matrix and creates a list capable of storing itself 
## and its inverse.
## functions are 'set','get','setinv','getinv'.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## cacheSolve takes a list created with makeCacheMatrix, 'x', and returns a matrix 
## that is the inverse of 'x$get()'. It does so without calculating the 
## inverse if it has already done so; calculating and storing in 'x' if is has not. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data)
  x$setinv(inv)
  inv

}
