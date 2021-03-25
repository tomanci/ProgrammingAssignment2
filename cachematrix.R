## Put comments here that give an overall description of what your
## functions do

## Here, this first function has the goal to store our object(matrix). makeCacheMatrix should be given back
## the matrix inverse to the users when they call it in the function below.

makeCacheMatrix <- function(x = matrix()) 
{
   inv<-NULL
   set<-function(y)
   {
    x<<-y
    inv<<-NULL
   }
   get<-function()
   {
           x
   }
        
   setinverse<-function(inv_matrix)
   {
          inv<<- inv_matrix
   }
   
   getinverse<-function()
   {
           inv
   }
   list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Here, in contrast with the previous function, is calculated the inverse. Indeed, first of all it is controlled that 
## the inv does not have a NULL value. 

cacheSolve <- function(x, ...) 
{
        inv<-x$getinverse()
        if(!is.null(inv))
        {                               
                message("getting cached data")  #here, this if checks whether inv has a previous value or not, if the answer is yes, it prints the value and exits from the function
                return(inv)
        }
        norm_matrix<-x$get()
        inv_matrix<-solve(norm_matrix, ...)
        x$setinverse(inv_matrix)
        inv
}
