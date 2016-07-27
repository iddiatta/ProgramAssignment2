### ProgramAssignment2.
### July 27-2016 
##  Author : Ibrahima  
MakeCacheMatrix <- function (x=matrix()){
        
## test if x is invertible 
         
   if (det(x)!=0)  {

        inv <- NULL 
        
        ## store a matrix 
        setmatrix <- function (y){
                x <<- y 
                inv <<- NULL
        }
        
        ## return the matrix stored 
        getmatrix <- function(){
                x
        } 
        
        ## cache he given argument 
        setinverse <- function(solve){
                inv <<- solve 
        } 
        
        ## return the cached matrix 
        getinverse <- function(){
                inv
        } 
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,setinverse= setinverse,
             getinverse = getinverse)
 }
        
}


## calculate the inverse of a matrix created with 
## the functon MakeCacheMatrix

CacheSolve <- function(x,...){
        
        ## inv get the value from MakeCacheMatrix
        inv <- x$getinverse()
        
        ## test the existence of the cahe matrix and return it if yes 
        if (!is.null(inv)){
                print("present in cache")
                return(inv)
        }
        
        ## if not data stored the matrix
        data <- x$getmatrix()
        
        ## and inv stored the inverse matrix calculated
        inv <- solve(data)
        
        x$setinverse(inv)
        
        # return the inverse 
        inv 
}


