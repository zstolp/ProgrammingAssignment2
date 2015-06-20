## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix makes a "special" matrix which is a list of functions

makeCacheMatrix <- function(x = matrix()) {
        #where x is a invertable square "special"matrix
        #returns a list with the functions:
        #1.) set value of matrix
        #2.) get value of matrix
        #3.) set the value of inverse of matrix
        #4.) get the value of inverse of matrix
        m<-NULL
        
        #1.) set value of matrix
        set<-function(y){
                x<<-y #<<- assigns value to object x(outside of current environment)
                m<<-NULL
        }
        #2.) get value of matrix
        get<-function()x
        #3.) set the value of inverse of matrix
        setm<-function(inverse)m<<-inverse
        #4.) get the value of inverse of matrix
        getm<-function()m
        #return function list
        list(set=set,get=get,setm=setm,getm=getm)

}


## checks to see if matrix was solved in cache, if not solve the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # x is output from makeCacheMatrix()
        # return inverse of the matrix input to makeCacheMatrix() function
        m<-x$getm()
        
        # if the matrix has been solved(the inverse)
        if(!is.null(m)){
                # retrieve from cache (skip the computation) and print to console
                message("getting cached data")
                return(m)
        }
        
        # if it has not been solved, solve the matrix (find the inverse)
        data<-x$get()
        m<-solve(data,...)
        
        # sets the value of solved matrix into cache
        x$setm(m)
        m #print m
}
