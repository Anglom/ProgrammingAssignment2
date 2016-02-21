## R Programming assignment 2
## Create a pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    
    set<-function(y){
      x<<-y
      s<<-NULL
    }
    
    get<-function()x
    setinverse<-function(inverse)s<<-inverse
    getinverse<-function() s
    
    list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the "matrix" creates by makeCacheMatrix
## If the inverse has already been computed the inverse is retrieved from the cache
cacheSolve <- function(x, ...) {
  
  s<-x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  data<-x$get()
  #data
  
  s<-solve(data,...)
  
  x$setinverse(s)
  s
}
