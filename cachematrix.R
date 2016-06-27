## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mGlobal = matrix()) {
  mInv<-NULL
  set <- function(y) {
    if(mGlobal!=y){
       mGlobal <<- y
       mInv <<- NULL
    }
  }
  get <- function() mGlobal
  setinv <- function(inv) mInv <<- inv
  getinv <- function() mInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(mGlobal, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- mGlobal$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # calcula la inversa
  data <- mGlobal$get()
  #print(data)
  m <- solve(data)
  #print(m)
  mGlobal$setinv(m)
  m
}
