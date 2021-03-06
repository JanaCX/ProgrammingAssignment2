# The following two functions help saving computation time by checking in memory for an existing result before doing additional calculations.

# To use these funcitons, call makeMatrix and cachesolve to the working environment. 
# Create a square matrix (requirement for the matrix to be "invertible"), p.e Variable1 is a non-singular square matrix
# >dummyVar1<-makeMatrix(Variable1)
# >dummyVar2<-cachesolve(dummyVar1)
# >print (dummyVar2)
# any time the cachesolve(dummyVar2) is called after the first time, 
# the function retrieves the value stored in memory instead of 
# recalculating the inverse of Variable1
#
#
# This first function creates the "decision list", when the second function runs 
# the elements in this list will tell whether the inverse has been calculated or not

makeMatrix<-function (mtx=matrix()){
      inv<-NULL
      set<-function (y){
            mtx<<-y      #sets the values of mtx and inv from outside the "set" function environment
            inv<<-NULL
      }
      get<-function() mtx
      setinverse<-function(inverse) inv<<-inverse
      getinverse<-function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
#
# checking for the inverse matrix in memory / calculating the inverse 
# the function calculates the inverse depending on the contents of the list created by makeMatrix
# if the element of the list called "setinverse" is not null (meaning it contains the result of calculating the inverse using "solve", the "cachesolve" function knows that the calculation has been performed and just retrieves the info stored in that element of the list, otherwise it calculates the inverse caling the function "solve"
#
cachesolve<-function (mtx, ...){
      inv<-mtx$getinverse()
      if(!is.null(inv)){
            message("getting cache data")
            return (inv)
      }
      data<-mtx$get()
      inv<-solve(data, ...)
      mtx$setinverse(inv)
      inv
}
