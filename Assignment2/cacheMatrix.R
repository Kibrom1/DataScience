cacheMatrix <- function(x = matrix()) {
  mInv<-NULL
  set<-function(y){
    x<<-y
    mInv<<-NULL
  }
  get<-function() x
  setInversematrix<-function(solve) mInv<<- solveMatrix
  getInversematrix<-function() mInv
  list(set=set, get=get,
       setInversematrix=setInversematrix,
       getInversematrix=getInversematrix)
}

cacheSolve <- function(x=matrix(), ...) {
  mInv <- x$getInversematrix()
  if(!is.null(mInv)){
    return(mInv)
  }
  matrix<-x$get
  mInv<-solve(matrix, ...)
  x$setInversematrix(mInv)
  mInv
}