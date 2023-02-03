## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x    #This function helps us to get matrix X
  setinv <- function(inverse) inv <<- inverse
  getinv <- function(){
    inver<-ginv(x)
    inver%*%x        #This function helps us to get the invers of the matrix X
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve helps us to get the cache data

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    return(inv)         ## Return a matrix that is the inverse of 'x'
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
