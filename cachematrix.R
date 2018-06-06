## This function creates a special "matrix" object
##    that can cache its inverse


makeCacheMatrix <- function(mymatrix = matrix())
  #constructor
{
  mymatrixInverse <- NULL
  get <- function()
  {
    mymatrix
  }
  set <- function(y)
  {
    mymatrix <<- y
    mymatrixInverse <<- NULL#Since it needs recalculation
  }
  
  getInverse <- function()
    mymatrixInverse
  setInverse <- function(x)
    mymatrixInverse <<- x
  
  list(get=get, set=set,getInverse= getInverse,setInverse= setInverse)
}


## This function computes the inverse of the special
##    "matrix" returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##    `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mymatrixInerse<-x$getInverse()
 
  if(!is.null(mymatrixInerse)) {
    message("getting cached data")
    return(mymatrixInerse)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
  
}
