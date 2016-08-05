## Set the matrix in the argument as a global variable if it were not previously saved
## mGlobal: a matrix to be saved as global 
## NOTE: Puts the matrix inverse variable to 'null' if it was not  allready saved 
makeCacheMatrix <- function(mGlobal = matrix()) {
  mInv<-NULL
  # Inner function that saves the matrix in case of not being previously as a global o differs from that
  set <- function(y) {
    if(mGlobal!=y){
       mGlobal <<- y
       mInv <<- NULL
    }
  }
  # Inner unction that returns the global matrix o null if there is no variable
  get <- function() mGlobal
  setinv <- function(inv) mInv <<- inv
  getinv <- function() mInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Computes the inverse of the matrix guven in the argument
## mGlobal: list representing a matrix and its inverse as a global variable
cacheSolve <- function(mGlobal, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- mGlobal$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # computes the inverse of the matrix
  data <- mGlobal$get()
  #print(data)
  m <- solve(data)
  #print(m)
  # saves the inverse in the global list
  mGlobal$setinv(m)
  m
}
