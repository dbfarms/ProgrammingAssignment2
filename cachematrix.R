## Put comments here that give an overall description of what your
## functions do

##The makeCacheMatrix creates the set, get, setmatrix, getmatrix functions.
##These are accessible via the list(...).

##cacheSolve checks for a cached matrix. If found (i.e. not NULL), it returns
##the value. If not, it calculates a new value and cahces it for later access.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x<<-y
      m<<-NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  
  
  if(!is.null(m)) {
    message("getting cached data")
      return(m)
  }
  
  data <- x$get() 
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}