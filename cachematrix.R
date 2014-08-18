## cachematrix.R
## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function
## function which returns a list of functions regarding matrix construction, getting the matrix back, setting the value of the inverse and getting back the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # initially inverse is NULL
  set <- function(y){
    # function to set matrix & it's inverse
    x <<- y
    s <<- NULL
  }
  get <- function() x # function to get matrix
  setInv <- function(xInv) s <<- xInv # function to set inverse matrix from xInv
  getInv <- function() s # function that returns inverse matrix
  list(set=set, get=get, setInv=setInv, getInv=getInv) # function which returns a list
}

## Write a short comment describing this function
## function which returns the inverse of a matrix; if it's cached already, then it doesn't estimate it again, however if it's not cached, it'll estimate it and return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  s <- x$getInv() # verify if inverse is cached
  if(!is.null(s)){
    # if inverse is cached, then return cached data to avoid computation
    message("getting cached data")
    return(s)
  }
  data <- x$get() # if it's not cached, get matrix
  s <- solve(data) # get inverse with solve function
  x$setInv(s) # set inverse value to s
  s
}

if(0){
  i<-5; x <- matrix(rnorm(i^2),i,i)
  solve(x)
  
  xx <- makeCacheMatrix(x)
  cacheSolve(xx)
  xx$get()
  xx$getInv()
  identical(cacheSolve(xx),solve(x))
}