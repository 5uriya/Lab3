#' Greatest Common Divisor[GCD] of two numbers
#' 
#' @param dividend A number
#' @param divisor A number
#' @return The GCD of two numbers
#' @examples 
#' euclidean(25,2)
#' euclidean(30,6)

euclidean <- function(dividend,divisor)
{
  dividend<-abs(dividend)
  divisor<-abs(divisor)
  if(divisor>dividend){
  
  dummy_variable <- divisor
  
  divisor <- dividend
  
  dividend <- dummy_variable
}
  if(divisor == 0){
    
    if(dividend == 0){
      
      return(0)
    }else{ return(dividend)}
  }
  
  while (divisor != 0)
  {
    temporary_variable <- divisor
    
    divisor <- dividend %% divisor
    
    dividend <- temporary_variable
  }
  
 return(temporary_variable)
}
euclidean(12,30)