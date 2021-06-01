## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# function creates a special "matrix" which is really a list containing functions to 1) set the value of the 
# matrix, 2) get the value of the matrix, 3) set the value of the matrix's inverse (i), and 4)
# get the value of the matrix's inverse 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# calculates the inverse of the special "matrix" created with the above function; skips the computation if 
# the inverse has already been computed and subsequently cached; otherwise, it calculates the inverse 
# of the matrix and sets the value of the inverse in the cache via the setinverse() function
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
