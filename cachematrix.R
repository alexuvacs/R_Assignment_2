## Below are two functions that are used to create a special object that stores a 
## numeric matrix and caches its inverse using the solve() function

## The first function makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to 
## (1) set the value of the matrix with 'set'         (2) get the value of the matrix with 'get' 
## (3) set the value of the inverse with 'setInverse' (4) get the value of the inverse with 'getInverse'

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) m<<- solve
  getInverse<-function() m
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## The second function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it returns the chached inverse 'm' and skips the rest of the computation 
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setInverse function to the variable 'm'

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setInverse(m)
  m
}
