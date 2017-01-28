## R Programming, Assignment # 2


## These two functions will take a matrix x as input and through lexical xcoping, allow the inverse of the matrix to be stored in 
## memory so that it can be called from the cache instead of being recalculated each time it is needed.
## 
## The first function, makecacheMatrix reads in the matrix x, creates a NULL value for it's inverse (m), and then creates a list
## of four functions that can be called from another function.  The values of x and m are retained in the makecacheMatrix environment.
##
## The second function, cachesolve, checks to see if the value of m in the makecacheMatrix environment is NULL or not.  If it
## is, it calculates the inverse value and stores it as m in the makecacheMatrix environment.  If it is not NULL, it just returns
## the value of m it finds.



## This function will take a matrix "x" as input and create a list of four new functions that can be
## called using the "$" operator in another funcion.

makecacheMatrix <- function(x = matrix()) {    # initialize objects x and m
  m <- NULL
  set <- function(y) {                         # create set function which assigns values for 
    x <<- y                                    # x and m (NULL) in the makecacheMatrix environment
    m <<- NULL
  }
  get <- function() x                          # create get function to get value of x from makecacheMatrix environment
  setinv <- function(solve) m <<- solve        # create set funcion to set the value of m in makecacheMatix environment
  getinv <- function() m                       # create get function to get the value of m from makecacheMatrix environment
  list(set = set, get = get,                   # create a list of the four funcions with names so they can be accessed with 
       setinv = setinv,                        # $ operator
       getinv = getinv)
}

## This function requires an argument that has been returned from makecacheMatrix - the list of functions.
## I checks to see if the value of m is NULL in the makecacheMatrix environment.  If it is not, it returns the value of m.  If it is,
## it calculates the inverse of the matrix, stores it as m in the makecacheMatrix environment, then returns m.

cachesolve <- function(x, ...) {              # initialize object that is the list created by makecacheMatrix, along with values
  m <- x$getinv()                             # for x and m
  if(!is.null(m)) {                           # if m is not NULL print to console "getting cached data" and return m
    message("getting cached data")
    return(m)
  }
  data <- x$get()                             # if m is NULL, get matrix x from makecacheMatrix environment
  m <- solve(data, ...)                       # use solve function for calculate matrix inverse and store if to m
  x$setinv(m)                                 # use setinv() function from makecacheMatrix to store value of m in makecacheMatrix
  m                                           # environment and return m
}


## Test the function using a simple invertable matrix:

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
