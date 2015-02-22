## Functions that enable caching the inverse of a matrix
## 

## This is a function that creates a special "matrix" object that can cache its inverse.
## It creates a special vector that contains the names of the following four functions. 
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. setinverse - set the inverse matrix
## 4. getinverse - get the inverse matrix
## example: >mattest<-matrix(c(3,2,0,0,0,1,2,-2,1),3,3)
##          > mattest
##           [,1] [,2] [,3]
##           [1,]    3    0    2
##           [2,]    2    0   -2
##           [3,]    0    1    1
##          >matexec<-makeCacheMatrix(mattest)


makeCacheMatrix <- function(x = matrix()) {
   inv <-NULL
   
   set <- function(y) {
   
     x <<-y
   
     inv<<-NULL
    }
   
   get<-function() x
   
   setinverse<-function(solve) inv<<-solve
   
   getinverse<-function() inv
   
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
   
}



## This is a functiion that calculates the inverse of a matrix.
## If the inverse matr##ix has already been calculated it takes the calculated value from the cache
## example continued:
## > cacheSolve(matexec)
## getting cached data
## [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    
     message("getting cached data")
     return(inv)
     
  }
  
  data<-x$get()
  
  inv<-solve(data,...)
  
  x$setinverse(inv)
  
  inv
}


