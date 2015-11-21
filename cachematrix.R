## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## We are writing a function to create a special matrix(square) which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## x is a square matix
  inv = NULL
  ## set a matrix
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  ##get the matrix
  get = function() x
  ## set the inverse
  setinv = function(inverse) inv <<- inverse 
  ## get the inverse
  getinv = function() inv
  ##list of set the matrix, get the matrix, setting the inverse and getting the inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
## This function computes the inverse of the special (square) matrix created by teh makeCacheMatrix and checks if
##its inverse exists, else cacheSolve will return the inverse of the matrix

cacheSolve <- function(x, ...) {
  ##x here will be the output of the function makeCacheMatrix
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # gets it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # else, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
        ## Returns a matrix that is the inverse of 'x'
}



## input to check the programs above
## x <- rbind(c(1, 2), c(3, 4))
##m1 <- makeCacheMatrix(x)
##m1$get() gets the matrix
##i1 <- cacheSolve(m1)
##i1
