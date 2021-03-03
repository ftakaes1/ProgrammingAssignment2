## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     In <- matrix()
     frame <- data.frame(FuncNames=1:4, row.names= c("setMx","getMx","setIn","getIn"))
     setMx <- function(y){
          x <<- y
          In <<- matrix()
     }
     getMx <- function(){
          return(x)
     }
     setIn <- function(Inverse) {
          In <- Inverse
     }
     getIn <- function(){
          return(In)
     }
     frame$FuncNames <- c(setMx,getMx,setIn,getIn)
     frame
     
     
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     In <- x[["getIn","FuncNames"]]()
     if(!is.na(In[[1,1]])){
          message("getting cached inverse matrix")
          return(In)
     }
     data <- x[["getMx","FuncNames"]]
     In <- solve(data())
     x[["setIn","FuncNames"]](In)
     return(In)
}
