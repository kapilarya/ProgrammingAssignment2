## There are two functions created in this project;makeCahceMatrix and cacheSolve 
## The makeCacheMatrix initializes matrix and stores the value of inverse while 
## cacheSolve returns the value of the inverse and computes and returns the 
## inverse


## makeCacheMatrix takes in a matrix as an argumet and creates an object
## that can store the value of the inverse of matrix. The function also has sub
## functions to get the value of the original matrix, set the inverse and get the
## inverse matrix when called from the cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse of equal to NULL
  m <- NULL
  # get the original matrix  
  get <- function() x
  # set invmat equal to the inverse computed in cacheSolve
  setinv <- function(data) m <<- data
  getinv <- function() m
  # get invmat once it has been computed
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes an invertible matrix as an argument. It then checks to see
## if the matrix inverse has already been computed. If yes,the inverse of matrix
## returned. If not, the inverse is computed and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the inverse matrix of the object by function call
  m <- x$getinv()
  # check to see if invmat is previously computed and not null. If not null then
  # display and message and matrix inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # get the original matrix by function call
  data <- x$get()
  # compute the inverse of matrix
  m <- solve(data)
  x$setinv(m)
  m
}
