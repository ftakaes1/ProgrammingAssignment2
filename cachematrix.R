## cachematrix.R contains two complementary functions that cache the inverse of a matrix,
## and they can also calculate the inverse of an existing matrix, display it, and cache it for easier
## retrieval.  

## makeCacheMatrix takes an existing matrix as an argument (if none are given, it defaults to a 1X1 matrix),
## and returns a data frame of functions that can store the argument for future use, replace the argument
## for another matrix, store the inverse of the argument, and set the inverse of the argument. 

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
     return(frame)
     
     
}


## cacheSolve takes makeCacheMatrix and either retrieves the cached inverse of a matrix,
## or it calculates the inverse of the matrix stored in the returned data frame from makeCacheMatrix. 

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
