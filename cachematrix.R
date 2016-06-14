## makeMatrix function creates a matrix, and has set , get methods
makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}

##cacheInverse computes the inverse of a matrix
cacheInverse <- function(x, ...){
  invr <- x$getinverse() ##trying to get the cached inverse
  if(!is.null(invr)){ ##if we get the cached inverse (i.e) not null
    message("getting cached inverse")
    return(invr)
  }
  data <- x$get() ## this is because x might have changed by calling set func in makematrix
                  ## so it gets the current x value
  invr <- solve(data,...) ##if cached data not found, compute inverse using solve()
  x$setinverse(invr) ##set the inverse value as cached data
  invr
}
