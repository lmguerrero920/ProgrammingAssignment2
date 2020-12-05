makeCacheMatrix <- function(x = matrix()) 
{
  # Inicializar la matriz a NULL

  inverse  <- NULL

  set <- function(y) 

  {

    x <<- y

    inverse <<- NULL

  }


  get <- function() 
  {
    x
  }
  
  setinverse <- function(inv)
  { 
    inverse <<- inv
  }
  getinverse <- function()
  {
    inverse
  } 


  list(set = set, get = get,

       setinverse = setinverse,

       getinverse = getinverse)

}


cacheSolve <- function(x, ...) 
{


  inverse <- x$getinverse()

  
  if(!is.null(inverse)) 

  {

    message("getting cached data")

    return(inverse)


  }
  
  data <- x$get()

  inverse <- solve(data, ...)

  x$setinverse(inverse)

  inverse
}


test <- function() 
{

  my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

  my_matrix$get()

  my_matrix$getinverse()

  cacheSolve(my_matrix)

  my_matrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modificaf  matriz existente


  cacheSolve(my_matrix)   # procesos, cache,y retorna nueva matriz inversa


  my_matrix$get()         # Retornar matrix


  my_matrix$getinverse()  # Retorna la matriz inversa


  my_matrix$get() %*% my_matrix$getinverse() # retorna la matriz de identidad
}