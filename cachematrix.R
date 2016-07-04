## The function makeCacheMatrix has four methods. 
## The four methods open the matrix as x, calculates an inverse,
##    then stores the inverse matrix as I and caches it. 
## By calling the set function the inverse matrix I is erased 
##    and after calculation it is replaced by a new value.

makeCacheMatrix <- function(x = matrix()){
  
  ## init inverse property
  I <- NULL
  
  ## set the matrix x
  set <- function(m){
    x <<- m
    I <<- NULL
  }
  
  ## get the matrix x
  get <- function(){
    x               
  } 
  
  ## set the inverse matrix of x
  setInv <- function(i) {
    I <<- i
  }
  
  ## get the inverse matrix of x
  getInv <- function() {
    I
  }
  
  ## list of internal methods
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The cacheSolve function calculates an inversion of the input matrix 
##    and stores it as the 'special' matrix CM. 

## If the inverse matrix CM has not yet been calculated or changed, 
##    then the inverse matrix IM of the matrix CM is calculated and
##    returned as the result. If the matrix CM has been already calculated 
##    then it is retrived from the cached version

cacheSolve <- function( x, ...){
  ## Return a matrix that is the inverse of x
  
  ## the inversion matrix IM of the matrix x
  IM <- x$getInv()
  
  ## calculation of the inversion matrix IM for new or
  ## changed matrix x
  if (is.null(IM)) {
    message('Calculating the inverse...')
    
    data <- x$get()
    IM <- solve(data, ...)
    x$setInv(IM)
  } else {
    message('Inverse cached ...')
  }
  
  IM
}
