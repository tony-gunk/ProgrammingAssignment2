## Functions to cache the inverse of a matrix

## Creating a special matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  #set the matrix
  set<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  #get the matrix
  get<-function(){
    m #return the matrix
  }
  #set the inverse of matrix
  setInv<-function(inv){
    i<<-inv
  }
  #get the inverse of matrix
  getInv<-function(){
    i
  }
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Calculate inverse of the given matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInv()
  #return the inverse if alredy set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #get matrix from the object
  data<-x$get()
  #calculate inverse
  m<-solve(data)%*%data
  #set inverse to object
  x$setInv(m)
  #return the matrix
  m
}
