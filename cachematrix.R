## Put comments here that give an overall description of what your
## functions do
## --- These functions combine to make a cached version of the inverse of a matrix --- 


## Write a short comment describing this function
## --- The function "makeCacheMatrix" creates a special list that 
## emulates a matrix that defines a set of functions that allow 
## users of the list to get and set the stored inverse ("inv"), and the underlying 
## matrix ("x") ---

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL

  set<-function(y){
    x<<-y
    inv<<-NULL
  }

  get<-function(){
    x
  }

  setinverse<-function(inverse){
    inv<<-inverse
  }

  getinverse<-function(){
    inv
  }

  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
## --- This function utilizes the special list created in "makeCacheMatrix" to compute and return the 
## inverse of a matrix. If cached inverse of the matrix if available, a message of 
## getting cached data is printed; If it is not available, this function then computes 
## and caches the inverse of the matrix --- 


cacheSolve <- function(x, ...) {
  invMatrix<-x$getinverse()
  
  if(!is.null(invMatrix)){
    message("getting cached data")
  }
  else {
    Matrix<-x$get()
    invMatrix<-solve(Matrix)
    x$setinverse(invMatrix)
  }
        ## Return a matrix that is the inverse of 'x'
  return(invMatrix)
}
