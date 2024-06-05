##I added my comments within the functions as the code occurs

##makeCacheMatrix stores the inverse of a matrix when run in conjunction with cacheSolve
##this allows computation of inverse matrices to be faster if they have already been computed within the environment previously

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  
  ##initialization of two objects:
  ##x a function argument defined as an empty matrix
  ##v an object in the makeCacheMatrix environment
  
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  
  ##assigns y to the x object in parent enviornment
  ##assigns NULL to v in parent environment
  ##clears any previous value of v cached by prior run
  ##allows you to subset using $, after defining list below, and reset new matrix
  ##must be sure matrix is square in order for inverse to exist if not singular
  ##so use rbind with n vectors of length n
  
  get <- function() x
  
  ##x is retrieved from the parent environment of makeCacheMatrix since not defined explicitly in get function
  
  setinverse <- function(inverse) {
    v <<- inverse
  }
  
  ##assigns input value of inverse to v in the parent environment, not just in this function, since double arrow operator is used
  
  getinverse <- function() v
  
  ##retrieves value of v from parent environment of makeCacheMatrix since not defined explicitly in getinverse function
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

  ##assigns the four functions above as elements of a list so that each element is named
  ##allowes use of the $ operator to access functions by name


##cacheSolve calls up a previously computed inverse of a matrix when run in conjunction with makeCacheMatrix

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  
  ##initialization of two objects
  ##function with x and possibly other inputs as allowed by the ellipsis
  ##attempt to retrieve inverse matrix of x passed into function
  
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  
  ##if inverse matrix has already been computed, which occurs if is.null(v) is FALSE - meaning v is not null and has a value
  ##prints message letting you know inverse has been cached and retrieves
  ##returns already computed inverse matrix v
  
  data <- x$get()
  
  ##if is.null(v) is TRUE, no message is displayed
  ##new matrix is "got" and stored as object called data
  
  v <- solve(data, ...)
  
  ##v is set to be the solved inverse matrix of matrix defined in x
  
  x$setinverse(v)
  
  ##sets inverse in x to be v
  
  v
  
  ##returns and prints inverse matrix
}
