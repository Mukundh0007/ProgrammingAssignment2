## Put comments here that give an overall description of what your

#makeCacheMatrix()<==creates a special "matrix" object that caches its inverse i.e. duplicates it in a global environment
#cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix and if the inverse has already been calculated, then the 
#            cachesolve retrieves the inverse from the cache.


## functions to do-makeCacheMatrix,cacheSolve

#Making the Cache Matrix function
makeCacheMatrix<-function(m=matrix()){
  invrs<-NULL
  set<-function(n){
    m<<-n
    invrs<<-NULL
  }
  get<-function(){m}
  setInv<-function(inverse){invrs<<-inverse}
  getInv<-function(){invrs}
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


#Solving the Cache Matrix
cacheSolve<-function(m,...){
  invrs<-m$getInv()
  if(!is.null(invrs)){
    message('Getting cache data')
    return(invrs)
  }
  matrx<-m$get()
  invrs<-solve(matrx,...)    #solve() returns the matrix inverse as output
  m$setInv(invrs)
  invrs   ## Returns a matrix that is the inverse of 'm'
}
