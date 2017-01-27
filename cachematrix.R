## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makecacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cachesolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


## Test the function

d<-matrix(c(3,0,0,3), 2, 2)  ## Create an invertable matrix

d                            ## Show matrix

solve(d)                     ## use solve() to calculate the inverse of d

s<-makecacheMatrix(d)        ## create cache inverse creating vector

cachesolve(s)                ## calculate initial inverse (no cache)

cachesolve(s)                ## calculate inverse a second time (calls from cache)

d1<-cachesolve(s)            ## store inverted matrix to d1

s1<-makecacheMatrix(d1)      ## create cache inverse creating vector

cachesolve(s1)               ## calculate initial inverse (no cache, should be back to original matrix)

cachesolve(s1)               ## calculate inverse a second time (calls from cache)
