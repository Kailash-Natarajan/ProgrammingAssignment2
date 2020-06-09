#makeCacheMatrix makes a matrix object that also contains its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse<-NULL
  
  set<-function(y){
    ##To set values to the matrix object
    x<<-y
    mat_inverse<<-NULL
  }
  get<-function(){
    ##To return values of the matrix
    x
  }
  setinverse<-function(inv){
    ##To set values of inverse parameter
    mat_inverse<<-inv
  }
  getinverse<-function(){
    ##To return values of the inverse parameters
    mat_inverse
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## cacheSolve when called sets inverse of the matrix object if not already present
## and returns the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
     inv<-x$getinverse()
     ##Checking if inverse is already present
     if(!is.null(inv))
       { ##inverse value is already present
       print("Getting value from cache...\n")
       return(inv)
     }
     ##Inverse is not present
     else
     { 
      print("Setting and printing the value of inverse")
      inv<-solve(x$get())
      x$setinverse(inv)
      return(inv)
     }
  }
