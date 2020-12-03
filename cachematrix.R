## Put comments here that give an overall description of what your
## functions do

#There are two functions makeCacheMatrix, makeCacheMatrix
#makeCacheMatrix consists of set, get, setinv, getinv
#library(MASS) is used to calculate inverse for non squared as well as squared matrices 
## Write a short comment describing this function
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                     #initializing inverse as NULL
  set<-function(y){
          x<<-y
          inv<<-NULL
  get<-function()x                   #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
          inver<-ginv(x)
          inver%*%x           #function to obtain inverse of the matrix
          }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)        
  }
}


## Write a short comment describing this function
#This is used to get the cache data

cacheSolve <- function(x, ...) ##gets cache data
{
inv<-x$getinv()
if(!is.null(inv)){
        return(inv)

}
data<-x$get()
inv<-solve(data,...)
x$setinv(inv)
inv          
        ## Return a matrix that is the inverse of 'x'
}
