## makeMatrix function creates a matrix, and has set , get methods
makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ ##sets the elements of matrix
    x <<- y
    inv <<- NULL ##sets the initial values of matrix to NULL
  }
  get <- function()x ##get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse ##set the inverse of the matrix
  getinverse <- function() inv ##get the inverse of the matrix
  list(set = set,
       get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}

##cacheInverse computes the inverse of a matrix if the cached data is not available. if the matrix values are already cacjed, the cached 
##inverted value is returned
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
