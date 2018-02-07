## Glalk, 02-07-18, Lexical Scoping: Programming Assignment 2
## Basing code heavily on sample provided, which is:
##

################################################################
##get the value of the mean

##makeVector <- function(x = numeric()) {
 ## m <- NULL
 ## set <- function(y) {
 ##   x <<- y
 ##   m <<- NULL
##  }
 ## get <- function() x
 ## setmean <- function(mean) m <<- mean
 ## getmean <- function() m
  ##list(set = set, get = get,
   ##    setmean = setmean,
  ##     getmean = getmean)
##}
## The following function calculates the mean of the special "vector" 
##created with the above function. However, it first checks to see if the mean
##has already been calculated. If so, it gets the mean from the cache
##and skips the computation. Otherwise, it calculates the mean of the data 
##and sets the value of the mean in the cache via the setmean function.

##cachemean <- function(x, ...) {
##  m <- x$getmean()
##  if(!is.null(m)) {
##    message("getting cached data")
##    return(m)
##  }
##  data <- x$get()
##  m <- mean(data, ...)
##  x$setmean(m)
##  m
##}

################################################################

## This function creates a special "matrix" object that can cache its inverse.
##We will use the function solve()
##to compute the inverse and assume the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <-function(y) {
    x<<-y
    i<<-NULL
    
  }
  get <-function() x
  setinverse <-function(solve) i <<- solve
  getinverse <-function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been
##calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
  ## Return a matrix that is the inverse of 'x'
