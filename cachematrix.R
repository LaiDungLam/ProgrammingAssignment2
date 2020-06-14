## Computing the inverse of a given matrix is costly in terms of
## computation resources. Furthermore, it is the case that a inverse
## matrix could be use frequently many places in the program. In this
## case, if a inverse matrix is created, it should be cached for 
## later use so that computer resource can be saved for other tasks.

## this notion of computing and caching a inverse matrix is 
## demonstrated by the two following functions 


## the first function creates a matrix and store the its inverse

makeCacheMatrix <- function(x) {
  s <- NULL
  set <- function(y) { 
    x <<- y 
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve 
  getSolve <- function() s
  list(set = set, get = get, setSolve = setSolve, 
       getSolve = getSolve)
}


## the second function creates the inverse of the matrix returned
## by the first function. But before doing any computation, this 2nd
## function shall check if the inversed matrix has been computed
## before. If this is the case, it skips the computation and display 
## the inversed matrix. Otherwise, it shall compute the inverse of
## the matrix and save it. 

cacheSolve <- function(x) {
  solve <- x$getSolve() 
  if (!is.null(solve)) { 
    print("getting cached data")
    return(solve)
    }
  matrix <- x$get()   
  solve <- solve(matrix)
  x$setSolve(solve)
  solve
}
