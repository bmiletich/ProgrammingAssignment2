## Put comments here that give an overall description of what your
## functions do

## This function makes a matrix object that can have a stored "inverse" value, retrievable by the get functions.

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(z){
  x<<-z
  #clears inverse when you set a new value
  i<<-NULL
}
get<-function(){
  x
}

setInverse <- function(inverse){
  i <<- inverse
}
getInverse <- function() {
  i
}
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)

}

## This function checks to see if the inverse is already cached. If it is, it retrieves it from cache. If not, then it computes the inverse and attaches it to the inverse property on the original object. 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
