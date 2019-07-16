#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



#-----------------------------------Statistical Calculator that perform various Statistical Analysis-----------------------------------------------------------------------------------------------------------------------------------


#---------------------------------------------CREATED BY: TUSHAR(40) and VIHAL(46)-----------------------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

fact<-function(number)
{	if(number<=1)
  
{ 
  return(1)
  
}
  
  
  else
  {	
    return(number*fact(number-1))
  }	 
  
  
}

rerun_mod2<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module2())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}

rerun_mod3<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module3())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}

rerun_mod4<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module4())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}

rerun_mod5<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module5())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}

rerun_mod6<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module6())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}

rerun_mod7<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module7())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}

rerun_mod8<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module8())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}


rerun_mod9<-function()
{
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(Module9())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
}

SortRank <- function(data_minus_mu){
  
  temp=0
  a<- 0
  b<- 0  
  data_minus_mu_abs = abs(data_minus_mu) 
  for( i in 1:length(data_minus_mu))
  {
    for( j in i:length(data_minus_mu))
    {
      if(data_minus_mu_abs[i] > data_minus_mu_abs[j])
      {
        temp = data_minus_mu_abs[i]
        data_minus_mu_abs[i] = data_minus_mu_abs[j]
        data_minus_mu_abs[j] = temp
        
        temp = data_minus_mu[i]
        data_minus_mu[i] = data_minus_mu[j]
        data_minus_mu[j] = temp
      }
    }
  }
  return(data_minus_mu)
}

UserSortKruskal <- function(input){
  temp=0
  for( i in 1:length(input))
  {
    for( j in i:length(input))
    {
      if(input[i] > input[j])
      {
        temp = input[i]
        input[i] = input[j]
        input[j] = temp
        
        temp <- names(input[i]) 
        names(input)[i]<-paste(names(input[j]))
        names(input)[j]<-paste(temp)
      }
    }
  }
  return(input)
}


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Combination function

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


Combination<-function(total,base)
{
  if(base==0 || base==total)
  {
    return(1)
  }
  else
    
  {
    return((fact(total))/(fact(total-base)*fact(base)))
  }
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Module1:- Descriptive Analysis( Mean , mode ,median, variance, standard deviation, mean absolute deviation, range, quartiles, IQR, maximum, minimum, skewness,kurtosis)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module1<- function()
{
  
  set <- c()
  print("Enter data and and press q to exit")

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    
#to take input from user till he wants to give(integer only)
 
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
   repeat 
  {
    element <- readline(prompt = "enter a number : ")
    print(element)
    if(element == "q")
      break
    is.letter <- function(x) grepl("[[:alpha:]]", x)        # to check given input is not other than character 
    
    if(element != "" && is.letter(element) == FALSE)
    {
      element <- as.integer(element)
      set <- c(set,element)
    }
  }
  cat("\n \n The final data set on which operations are being performed is : ",set,"\n")
  set<- sort(set)

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

# to calculate mean

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  sumset <- 0
  index <- 1
  repeat
  {
    if(index > length(set))
      break
    sumset<- sumset+set[index]
    index<-index+1
  }
  mean <- sumset/length(set)
  cat("\nMean is",mean,"\n")
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
   
#to calculate median of given data
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  median <- set[(length(set)/2)+1]
  cat("\nMedian : ",median,"\n")
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
#to calculate mode of given data

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
    getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  cat("\nMode : ",getmode(set),"\n")

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

#to calculate variance

    
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  varsum <- 0
  vari <- 1
  repeat
  {
    varsum <- varsum + (set[vari] - mean)^2
    vari <- vari+1
    if(vari > length(set))
      break
  }
  variance <- varsum/length(set)
  cat("\nVariance : ",variance)
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    
# to calculate Standard Deviation
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  SDeviation <- sqrt(variance)
  cat("\nStandard deviation :",SDeviation,"\n")

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

#to calculate absoute mean deviation

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
    absum <- 0
  abindex <-1
  repeat
  {
    if(abindex > length(set))
      break
    val <-abs(mean - set[abindex])
    absum <- absum+val
    abindex <- abindex+1
  }
  abdeviation <- absum/length(set)
  cat("\nAbsolute mean deviation : ",abdeviation,"\n")
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
#to calculate range

    
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  minimum <- set[1]
  maximum <- set[length(set)]
  range <- maximum - minimum
  
  cat("Maximum of set : ",maximum,"\n")
  cat("Minimum of set : ",minimum,"\n")
  cat("Range of set :",range,"\n")

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
    
#to calculate quartile
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  q1 <- (set[length(set)])/4
  q2 <- (set[length(set)])/2
  q3 <- (3 * set[length(set)])/4
  cat("1st quartile :",q1,"\n")
  cat("2nd quartile :",q2,"\n")
  cat("3rd quartile :",q3,"\n")
  IQR <- (q3-q1)/2
  cat("IQR is : ",IQR,"\n")

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

    
#to calculate skewness and krutosis
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  skewness <- (mean - median)/SDeviation
  cat("Skewness is :",skewness)
  selection_module1<-function()
  {
    cat("\n1.Descriptive analysis on other data set " , "\n2.Main Module")
    choice<-readline(prompt = "Enter the Choice : ")
    if(choice==1)
    {
      return(Module1())
    }
    else if(choice==2)
    {
      return(MainModule())
    }
    else
    {
      print("Invalid Selection")
    }
  }
  select<-readline(prompt = "RE-RUN? (Y/N)?")
  if(select=='Y' || select == 'y')
  {
    return(selection_module1())
  }
  else if(select=='N'||select=='n')
  {
    print("Calculator closed")
  }
  
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Module2 :- PREDICTIVE ANALYSIS


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module2<-function()
{
  
  Correlation <-function()
  { 
    print("Enter input : ")
    input<-scan()
    print("Enter Input1 : ")
    input1<-scan()
    sum1<-0
    dinp1<-as.numeric()
    mu1<-mean(input)
    n1<-length(input)
    for(i in 1:n1)
    {
      y<-(input[i]-mu1)
      dinp1<-c(dinp1,y)
    }
    dinp2<-as.numeric()
    mu2<-mean(input1)
    n2<-length(input1)
    for(i in 1:n2)
    {
      y<-(input1[i]-mu2)
      dinp2<-c(dinp2,y)
    }
    dinp<-dinp1*dinp2
    for(i in 1:n1)
    { sum1<-sum1+dinp[i]
    }
    
    s1<-sd(input)
    s2<-sd(input1)
    s<-s1*s2
    r<-sum1/(s*(n1-1))
    return(r)
  }
    
  MultipleLinearRegression<-function()
  {
    n<-as.numeric(readline("total values:"))
    print("x1 : ")
    x1<-scan()
    print("x2 : ")
    x2<-scan()
    print("y : ")
    y<-scan()
    x1sq<-x1*x1
    x2sq<-x2*x2
    x1x2<-x1*x2
    x1y<-x1*y
    x2y<-x2*y
    x<-matrix(nrow=3,ncol=3)
    x[1,1]=n
    x[1,2]=sum(x1)
    x[1,3]=sum(x2)
    x[2,1]=sum(x1)
    x[2,2]=sum(x1sq)
    x[2,3]=sum(x1x2)
    x[3,1]=sum(x2)
    x[3,2]=sum(x1x2)
    x[3,3]=sum(x2sq)
    y1<-matrix(nrow=3,ncol=1)
    y1[1,1]=sum(y)
    y1[2,1]=sum(x1y)
    y1[3,1]=sum(x2y)
    coeff<-solve.default(x,y1)
    print(paste0("y= ",coeff[1,1]," + x1*",coeff[2,1]," + x2*",coeff[3,1]))
    
  } 
  
  selection_module2<-function()
  {
    cat("\nPREDICTIVE ANALYSIS", "\n1. Correlation ","\n2. Multiple Linear Regression","\n3. Main Module")
    choice<-readline(prompt = "Enter your choice : ")
    if(choice==1)
    {
      print("Correlation")
      print(Correlation())
      return(rerun_mod2())
    }
    else if (choice==2)
    {
      print("Multiple Linear Regression")
      print(MultipleLinearRegression())
      return(rerun_mod2())
    }
    else if(choice==3)
    {
      return(MainModule())
    }
    else
    {
      print("Invalid Selection ")
    }
  }
  selection_module2()
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Module3 :- Probability Analysis (Permutations,Combinations,Basic Probability,Conditional Probability,Bayes Theorem)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module3<-function()
{
  

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Permutation

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



Permutation<-function(total,base)

{
if(base==total)
   {
	      return(fact(total))
						}
else if(base==0)
	{
		return(1)
				}

else

	{
 	return(fact(total)/fact(total-base))
								}
									}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Basic Probability

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

UserBprobability <-function(sample,event){

  TotalOutcomes<-length(sample)
  PossibleOutcomes<-length(event)
  probability<-PossibleOutcomes/TotalOutcomes
  if(probability>1)
  {
    return("No such probability exists")
  }
  return(probability)
}

Bayes<-function(prior,like){
  sum<-0
  PABi <- prior * like
  for(i in 1:length(PABi))
  { 
    sum<-sum+PABi[i]
  }
  PBiA <- PABi/sum
  return(PBiA)
}

Intersection<-function(A,B,sample){
  out<-as.numeric()
  n1<-length(A)
  n2<-length(B)
  for(i in 1:n1)
  {
    for(j in 1:n2)
    {
      if(A[i]==B[j])
      { 
        y<-A[i]
        out<-c(out,y)
      }
    }
  }
  n<-length(out)
  if(n == 0)
  {
    PA <- UserBprobability(sample,A)
    PB <- UserBprobability(sample,B)
    inter <- PA*PB
    
  }
  else
  {
    inter  = UserBprobability(sample,n)
  }
  return(inter)
}

ConditionalProb1<-function(A,given,sample){
  PB<-UserBprobability(sample,given)
  PAB<-Intersection(A,given,sample)
  AB<-PAB/PB
  AB
}

ConditionalProb2<-function(B,given,sample){
  PB<-UserBprobability(sample,given)
  PAB<-Intersection(B,given,sample)
  BA<-PAB/PB
  BA
}

condprob<- function(A,B,sample){
  
  
  return(c(ConditionalProb1(A,B,sample),ConditionalProb2(B,A,sample)))
  
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



#Category Selection for module 3



#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




selection_module3<-function()
{
  
  
  cat("\nEnter your Choice" ,"\n 1. Permutations", "\n 2. Combinations", "\n 3. Basic Probability","\n 4. Conditional Probabiltiy","\n 5. Bayes Theroem", "\n 6. Main Module")
  
  choice<-readline(prompt="Enter your choice :- ")
  
  if(choice == 1)
  {
    print("PERMUTATIONS")
    total<-as.numeric(readline(prompt = "Enter the Total :- "))
    base<-as.numeric(readline(prompt = "Enter the Base :- "))
    print(Permutation(total,base))
    return(rerun_mod3())
    
  }
  
  else if (choice==2)
  {
    print("COMBINATIONS")
    total<-as.numeric(readline(prompt = "Enter the Total :- "))
    base<-as.numeric(readline(prompt = "Enter the Base :- "))
    print(Combination(total,base))
    return(rerun_mod3())
  }
  else if (choice==3)
  {
    print("Basic Probability")
    print("Enter sample (Press Enter to exit read state) :")
    sample<-scan()
    print("Enter event (Press Enter to exit read state) :")
    event<-scan()
    print(UserBprobability(sample,event))
    return(rerun_mod3())
    
  }
  else if(choice==4)
  { print("CONDITIONAL PROBABILITY")
    print("Enter A (Press Enter to exit read state) :")
    A<-scan()
    print("Enter B (Press Enter to exit read state) :")
    B<-scan()
    print("Enter sample (Press Enter to exit read state) :")
    sample<-scan()
    print(condprob(A,B,sample))
    return(rerun_mod3())
  }
  else if(choice==5)
  { print("BAYES THEOREM")
    print("Enter Prior : ")
    prior<-scan()
    print("Enter like : ")
    like<-scan()
    print(Bayes(prior,like))
    return(rerun_mod3())
  }
  else if(choice==6)
  {
    
    return(MainModule())
    
  }
    else
    {
      print("Invalid Selection")
    }
    }
  selection_module3()
} 
  

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#MODULE4:  Discrete Distribution Functions (Uniform, Bernoulli, Binomial, Geometric, Hyper-geometric, Negative Binomial, Poisson, Multinomial, Multivariate Hypergeometric) 

          #Some Common Inputs :- X = Random Variable for all the distributions
          #          prob = probability for all the distributions
          

                     
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------




Module4<-function()
  {
    Bern<-function(X,prob)
      {if(X>1 || X<0)
        {
  return(0)
        }
  
  if(prob>1 || prob<0)
    {
    print("ERROR in Probability, Please check your inputs")
  }
      
  else 
  {
    return((prob^X)*(1-prob)^(1-X))
  }
      
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#Binomial Distribution

#Inputs: X = No. Successes obtained 
#        n = Total no. of bernoulli trails


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



Bin<-function(X,n,prob)
{
  
  if(prob>1 || prob<0)
  {
    print("ERROR in Probability, Please check your inputs")
  }
  else
  { return(Combination(n,X)*(prob^X)*(1-prob)^(n-X))
  }
}	



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#Geometric Distribution
#Inputs : successnum = very first success after a number of failures


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



Geomdist<-function(successnum,prob)
{
  
  if(prob>1 || prob<0)
  {
    print("ERROR in Probability, Please check your inputs")
  }
  else
  {
    
  
  return(prob*(1-prob)^(successnum-1))
  

  }
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#HyperGeometric (Applies to Sampling without replacement)
#inputs : Samplesize = Size of the sample selected
#         Totelements = size of the population
#         Successes   = No. of success occuring


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



hypgeo<-function(X,samplesize,Totelements,Successes)
{
  if(X<0)
  {
    print("Error(Random Variable (X)), Please check your inputs ")
  }
  else
    {
      return(Combination(Successes,X)*Combination(Totelements-Successes,samplesize-X)/Combination(Totelements,samplesize))
      
      
  }}
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#Negative Binomial

#Inputs : Successnum = nth Success to occur 



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------




NegBin<-function(X,Successnum,prob)
{
  
  if(prob>1 || prob<0)
  {
    print("ERROR in Probability, Please check your inputs")
  }
  
  else
  {
    return(Combination(X-1,Successnum-1)*prob^Successnum*(1-prob)^(X-Successnum))
    
  }
  
  
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#POISSON DISTRIBUTION
#Inputs : samplesize
#         lambda = samplesize*prob


#----------------------------------a------------------------------------------------------------------------------------------------------------------------------------




poiss<-function(X,lambda,prob,samplesize)
  
{
  lambda=samplesize*prob
  
  if(X<0)
  {
    
    print("Error in RandomVariable(X),Check your inputs")
    
  }
  else
  {
    return(lambda^X*exp(-lambda)/fact(X))
    
  }
  
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#Multivariate Hypergeometric

mulvarhypgeo<-function()
{ print("Enter number of Success states in all the samples(M), press Enter to Exit read state :")
  Successes<-scan()
  print("Enter the number of obsereved successes in all the samples(x), press Enter to Exit read state :")
  x<-scan()
  samplesize<-as.numeric(readline(prompt ="Enter the total no. of sample elements selected :- "))
  Totelements<-as.numeric(readline(prompt ="Enter total elements :- "))
  if(length(Successes)!=length(x))
  {
    print("\nError!,Recheck your data no. of Success states is not equal to no. of observed successes")
  }
  else
  {
  prod<-1
  for(i in 1:length(x))
  { 
    prod<-prod*Combination(Successes[i],x[i])
  }
   return(prod/Combination(Totelements,samplesize))
  }
}



Multinom<-function()
{ print("\nEnter successes from different samples(X1,X2,.....,Xk),press Enter to Exit read state")
  X<-scan()
  n<-as.numeric(readline(prompt = "Enter total outcomes :- "))
  print("Enter the probability for different samples for each individual success(< 1 ),press Enter to exit read state")
  prob<-scan()
  prod<-1
  prodX<-1
  if(length(X)!=length(prob))
  {
    print("\nERROR please check your data")
  }
  else
  {
    for(i in 1:length(X))
    {
      prod<-prod*(prob[i]^X[i])
      prodX<-prodX*fact(X[i])
    }
    return((fact(n)*prod)/prodX)
  }
  
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#category selection for module4


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



selection_module4<-function()
{
  

cat("\nEnter your Choice" ,"\n 1. Bernoulli", "\n 2. Binomial", "\n 3. Geometric", "\n 4. Hypergeometric", "\n 5. Negative Binomial" ,"\n 6. Poisson ","\n 7. Multivariate Hypergeometric", "\n 8. Multinomial", "\n 9. Main Module")

choice<-readline(prompt="Enter your choice :- ")

if(choice == 1)
{
print("BERNOULLI'S DISTRIBUTION")  
X<-as.numeric(readline(prompt ="Enter the Success (x) (0,1) :- "))
prob<-as.numeric(readline(prompt ="Enter the probability :- "))
print('Calculating...............')
print(Bern(X,prob))
return(rerun_mod4())
}

else if(choice == 2)
{ print("BINOMIAL DISTRIBUTION")
  X<-as.numeric(readline(prompt ="Enter the total no. of successes (x) :- "))
  n<-as.numeric(readline(prompt = "Enter total outcomes :- "))
  prob<-as.numeric(readline(prompt ="Enter the probability of each individual success :- "))
  print(Bin(X,n,prob))
  return(rerun_mod4())
  }
else if(choice==3)
{
  print("GEOMETRIC DISTRIBUTION")
  successnum<-as.numeric(readline(prompt = "Enter the trail for first success :- "))
  prob<-as.numeric(readline(prompt = "Enter the Probability :- "))
  print(Geomdist(successnum,prob))
  return(rerun_mod4())
  
}
else if(choice==4)
{
  print("HYPERGEOMETRIC DISTRIBUTION [ h(x,n,N,M)] ")
  X<-as.numeric(readline(prompt ="Enter the number of obsereved successes(x) :- "))
  samplesize<-as.numeric(readline(prompt ="Enter sample size(n) :- "))
  Totelements<-as.numeric(readline(prompt ="Enter total elements(N) :- "))
  Successes<-as.numeric(readline(prompt ="Enter number of Success states in population(M) :- "))
  print(hypgeo(X,samplesize,Totelements,Successes))
  return(rerun_mod4())
}

else if(choice==5)
{
  print("Negative Binomial Distribution")
  X<-as.numeric(readline(prompt ="Enter X :- "))
  Successnum<-as.numeric(readline(prompt ="Enter the trail on which Kth success occur :- "))
  prob<-as.numeric(readline(prompt ="Enter probability :- "))
  print(NegBin(X,Successnum,prob))
  return(rerun_mod4())
}
else if(choice==6)
{
  print("POISSON DISTRIBUTION")
  X<-as.numeric(readline(prompt ="Enter X :- "))
  prob<-as.numeric(readline(prompt ="Enter probability :- "))
  samplesize<-as.numeric(readline(prompt ="Enter sample size :- "))
  print(poiss(X,lambda,prob,samplesize))
  return(rerun_mod4())
}
else if(choice==7)
{
  print("MULTIVARIATE HYPERGEOMETRIC DISTRIBUTION")
  print(mulvarhypgeo())
  return(rerun_mod4())
}
else if(choice==8)
{
  print("MULTINOMIAL DISTRIBUTION")
  print(Multinom())
  return(rerun_mod4())
}
else if(choice == 9)
{
  return(MainModule())
}
else
{
  print("Invalid Selection")
}
  }
return(selection_module4())

}



#-----------------------------------------------------------------------------------------------------------------------------------------------


#Module5. CONTINUOUS DISTRIBUTIONS

Module5<-function()
{

#-----------------------------------------------------------------------------------------------------------------------------------------------



#UNIFORM DISTRIBUTION



#-----------------------------------------------------------------------------------------------------------------------------------------------




Uniform<-function(x,alpha,beta)
{
  if(x>=alpha && x<=beta)
  {
    return(1/(beta-alpha))
  }
else
  {
    return(0)
  }
}




#-----------------------------------------------------------------------------------------------------------------------------------------------


#NORMAL DISTRIBUTION


#-----------------------------------------------------------------------------------------------------------------------------------------------


normald<-function(x,mean,sd)
{if(x>-Inf && x<Inf)
{
  calc<-((x-mean)/sd)^2
  return((exp(-1/2*(calc)))/(sd*sqrt(2*pi)))
}
  
}




#-----------------------------------------------------------------------------------------------------------------------------------------------


#BIVARIATE NORMAL DISTRIBUTION


#-----------------------------------------------------------------------------------------------------------------------------------------------


bivnorm<-function(x,mean1,sd1,y,mean2,sd2,rho)
{
  z<-((x-mean1)/sd1)^2-(2*rho*((x-mean1)/sd1)*((y-mean2)/sd2))+((y-mean2)/sd2)^2
  if((x>-Inf && x<Inf) && (y>-Inf && y<Inf) && (rho>-1 && rho<1))
    {
      return((exp(-z/2*(1-rho^2)))/(2*pi*sd1*sd2*sqrt(1-rho^2)))
               
    }
    else
    {
      print("Error! rho must be between -1 and 1")
    }
  }


#-----------------------------------------------------------------------------------------------------------------------------------------------


#GAMMA DISTRIBUTION


#-----------------------------------------------------------------------------------------------------------------------------------------------

func<-function(alpha)
  {
  func2<-function(y){
    ((y^(alpha-1)*(exp(-y))))
  }
  return(func2)
}


integral<-function(alpha)
{
  if(alpha>0)
  {
    integrate(func(alpha),lower=0,upper=Inf)$val
  }
  else if(alpha==1)
  {
    return(1)
  }
}


Gamma<-function(x,alpha,beta)
{
  
  if(x>0)
  {
    return((x^(alpha-1))*exp(-x/beta)/((beta^alpha)*(integral(alpha))))
    
  }
  
  else
  {
    return(0)
  }
  
}

#-----------------------------------------------------------------------------------------------------------------------------------------------


#EXPONENTIAL DISTRIBUTION


#-----------------------------------------------------------------------------------------------------------------------------------------------


expo<-function(x,theta)
{
  if(x>0)
    {
    if(theta>0)
      {
    return((exp(-x/theta))/theta)
    }
    else
    {
      print("ERROR!theta must be greater than zero!!")
    }
  }
  else
  {
    return(0)
    
  }
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Category selection for Module5


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




selection_module5<-function()
{
  
  
  cat("\nEnter your Choice" ,"\n 1. Uniform", "\n 2. Normal", "\n 3. Bivariate Normal", "\n 4. Gamma", "\n 5. Exponential" ,"\n 6. Main Module")
  
  choice<-readline(prompt="Enter your choice :- ")
  
  if(choice == 1)
  {
    print("UNIFORM DISTRIBUTION")  
    alpha<-as.numeric(readline(prompt ="Enter Alpha :- "))
    x<-as.numeric(readline(prompt ="Enter x(greater than equal to alpha) :- "))
    beta<-as.numeric(readline(prompt ="Enter Beta(greater than equal to x) :- "))
    print(Uniform(x,alpha,beta))
    return(rerun_mod5())
  }
  
  else if(choice == 2)
  { print("NORMAL DISTRIBUTION")
    x<-as.numeric(readline(prompt ="Enter x :- "))
    mean<-as.numeric(readline(prompt = "Enter mean :- "))
    sd<-as.numeric(readline(prompt ="Enter standard deviation :- "))
    print(normald(x,mean,sd))
    return(rerun_mod5())
  }
  else if(choice==3)
  {
    print("BIVARIATE NORMAL DISTRIBUTION")
    x<-as.numeric(readline(prompt = "Enter the 1st variable :- "))
    mean1<-as.numeric(readline(prompt = "Enter the mean for 1st variable :- "))
    sd1<-as.numeric(readline(prompt = "Enter the standard deviation for 1st variable :- "))
    y<-as.numeric(readline(prompt = "Enter the 2nd variable :- "))
    mean2<-as.numeric(readline(prompt = "Enter the mean for 2nd variable :- "))
    sd2<-as.numeric(readline(prompt = "Enter the standard deviation for 2nd variable :- "))
    rho<-as.numeric(readline(prompt = "Enter Rho (-1,1) :- "))
    print(bivnorm(x,mean1,sd1,y,mean2,sd2,rho))
    return(rerun_mod5())
    
  }
  else if(choice==4)
  {
    print("GAMMA DISTRIBUTION ")
    x<-as.numeric(readline(prompt ="Enter x(>0) :- "))
    alpha<-as.numeric(readline(prompt ="Enter alpha :- "))
    beta<-as.numeric(readline(prompt ="Enter beta :- "))
    print(Gamma(x,alpha,beta))
    return(rerun_mod5())
  }
  
  else if(choice==5)
  {
    print("EXPONENTIAL DISTRIBUTION")
    x<-as.numeric(readline(prompt ="Enter x :- "))
    theta<-as.numeric(readline(prompt ="Enter theta :- "))
    print(expo(x,theta))
    return(rerun_mod5())
  }
  else if(choice == 6)
  {
    return(MainModule())
  }
  else
  {
    print("Invalid Selection")
  }
}
  return(selection_module5())
}


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Module6. SAMPLE DISTRIBUTION TEST STATISTICS


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module6<-function()
{
  cinterval<-as.numeric(readline(prompt = "Enter the confidence interval(in %age) : "))
  alpha<-(100-cinterval)/100
  
  
  ZTest<-function()
  { 
    sigma<-as.numeric(readline(prompt = "Enter the Standard Deviation : "))
    mu<-as.numeric(readline(prompt = "Enter the Sample mean : "))
    n<-as.numeric(readline(prompt = "Enter the Sample size : "))
    xbar<-as.numeric(readline(prompt = "Enter the Population mean : "))
    nullhyp<-readline(prompt = "Enter the null hypothesis : ")
    z_cal<-((xbar-mu)/(sigma/sqrt(n)))  
    z_obs1<-qnorm(alpha)
    cat("\nSelect your choice ","\n1. Two-Tailed ", "\n2. Right-tailed ", "\n3. Left-Tailed")
    choice<-readline(prompt = "Enter your choice : ")
    if(choice==1)
    {
      if(z_cal<=qnorm(alpha/2) && z_cal>=qnorm(alpha/2))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted" )
      }
    }
    else if(choice==2)
    {
      if(z_cal>=abs(qnorm(alpha)))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    else if(choice==3)
    {
      if(z_cal<=qnorm(alpha))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    else
    {
      print("Invalid Selection EXiting!!!")
    }
  }
  
  StudentTtest<-function()
  {
    print("Enter the data , press Enter to exit the read state : ")
    data<-scan()
    mu<-as.numeric(readline(prompt = "Enter the Sample mean : "))
    n<-length(data)
    var<-var(data)
    xbar<-mean(data)
    t_cal<-(xbar-mu)/((var^0.5)/sqrt(n))
    nullhyp<-readline(prompt = "Enter the null hypothesis : ")
    cat("\nSelect your choice ","\n1. Two-Tailed ", "\n2. Right-tailed ", "\n3. Left-Tailed")
    choice<-as.numeric(readline(prompt = "Enter your choice : "))
    if(choice==1)
    {
      if(t_cal<=qt(alpha/2,n-1) && t_cal>=qt(alpha/2,n-1))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    else if(choice==2)
    {
      if(t_cal>=abs(qt(alpha,n-1)))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    else if(choice==3)
    {
      if(t_cal<=qt(alpha,n-1))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    
    else
    {
      print("Invalid Selection!!  Exiting")
    }
  
    }
  
  
  
  ChiSqTest<-function()
  {
    print("Enter the data , press Enter to exit the read state : ")
    data<-scan()
    popVar<-as.numeric(readline(prompt = "Enter the population variance : "))
    n<-length(data)
    sampVar<-var(data)
    chi_cal<-(n-1)*sampVar/popVar
    nullhyp<-readline(prompt = "Enter the null hypothesis : ")
    cat("\nSelect your choice ","\n1. Two-Tailed ", "\n2. Right-tailed ", "\n3. Left-Tailed")
    choice<-as.numeric(readline(prompt = "Enter your choice : "))
    if(choice==1)
    {
      if(chi_cal<=qchisq(alpha/2,n-1) && chi_cal>=qchisq((1-alpha/2),n-1))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    else if(choice==2)
    {
      if(chi_cal>=qchisq((1-alpha),n-1))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    else if(choice==3)
    {
      if(chi_cal<=qchisq(alpha,n-1))
      {
        cat("Null hypothesis that ", nullhyp, "is Rejected ")
      }
      else
      {
        cat("Null hypothesis that ", nullhyp, "is Accepted ")
      }
    }
    else
    {
      print("Invalid Selection Exiting!!!!!!!")
      
    }
  }
  
  FDistTest<-function()
  {
    print("Enter the data 1 : ")
    data1<-scan()
    print("Enter the data 2 : ")
    data2<-scan()
    Svar1<-var(data1)
    Svar2<-var(data2)
    n1<-length(data1)
    n2<-length(data2)
    f_cal1<-Svar1/Svar2
    f_cal2<-Svar2/Svar1
    nullhyp<-readline(prompt = "Enter the null hypothesis : ")
    alterhyp<-readline(prompt = "Enter the alternate hypothesis : ")
    cat("\nSelect your choice ","\n1. Two-Tailed ", "\n2. Right-tailed ", "\n3. Left-Tailed")
    choice<-as.numeric(readline(prompt = "Enter your choice : "))
    if(choice==1)           
    {
      if(Svar1>=Svar2)
      {
        if(f_cal1>=qf(1-(alpha/2),n1-1,n2-1))
        {
          return("Variances are not equal!")
        }
        else
        {
          return("Variances are equal")
        }
      }
      else
      {
        if(f_cal2>=qf(1-(alpha/2),n2-1,n1-1))
        {
          return("Variances are not equal!")
        }
        else
        {
          return("Variances are equal")
        }
      }
    }
    else if(choice==2)       #right tail
    {
      if(f_cal1>=qf(1-alpha,n1-1,n2-1))
        return("Variance of population1 is greater then Variance of population2")
      else
        return("Variances are equal")
    }
    else if(choice==3)
    {
      if(f_cal2>=qf(1-alpha,n2-1,n1-1))  #left tail
        return("Variance of population1 is less then Variance of population2")
      else
        return("Variances are equal")
    }
    else
    {
      return("\nInvalid Selection Exiting!!!! ")
    }
  }
  selection_module6<-function()
  {
    cat("\nSelect your choice : ", "\n1. Z-test ", "\n2. Student t-test" , "\n3. Chi-Square Test","\n4. F-test","\n5. Main Module")
    choice<-readline(prompt = "Enter your choice : ")
    if(choice==1)
    {
      print("Z-TEST")
      print(ZTest())
      return(rerun_mod6())
    }
    else if(choice==2)
    {
      print("STUDENT T-TEST")
      print(StudentTtest())
      return(rerun_mod6())
    }
    else if(choice==3)
    {
      print("CHI-SQUARE TEST")
      print(ChiSqTest())
      return(rerun_mod6())
    }
    else if(choice == 4)
    {
      print("F-TEST")
      print(FDistTest())
      return(rerun_mod6())
    }
    else if(choice==5)
    {
      return(MainModule())
    }
    else
    {
      print("Invalid Selection")
    }
  }
  return(selection_module6())
  
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Module7. INTERVAL ESTIMATION


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module7 <- function()
{
  cinterval <- as.numeric(readline(prompt = "Enter confidence interval(in percentage) : "))
  alpha <- (100 - cinterval)/100
  cat("alpha is : ",alpha)
  zval<-abs(qnorm(alpha))
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
   
#Estimation of mean

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
    eom <- function()
  {
    number <-as.numeric(readline(prompt = "Enter the number of sample population : "))
    sdeviation <-as.numeric(readline(prompt = "Enter the standard deviation for sample : "))
    mean <- as.numeric(readline(prompt = "Enter the mean for sample : "))
    
    if(sdeviation == "")
    {
      cat("\n you have not entered standard deviation for sample")
      psdeviation <-as.numeric(readline(prompt = "Enter the standard deviation of population"))
      if(psdeviation == "")
      {
        print("\n Wrong value has been inserted .. calcualtor is exitting")
        Sys.sleep(2)
        return()
      }
      if(n < 30)
      { 
        lvalue <- (mean - zval*psdeviation)/number^(1/2)
        hvalue <- (mean + zval*psdeviation)/number^(1/2)
      }
      
      if(n > 30)
      { tval<-qt(alpha,number)
        lvalue <- (mean - tval*psdeviation)/number^(1/2)
        hvalue <- (mean + tval*psdeviation)/number^(1/2)
      }
    }
    
    lvalue <- (mean - zval*sdeviation)/number^(1/2)
    hvalue <- (mean - zval*sdeviation)/number^(1/2)
    
    cat("\nThe confidence interval lies between : ",lvalue," to ",hvalue)
  }
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
#Estimation of proportions
  

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
    
  eop <- function()
  {
    number<-as.numeric(readline(prompt ="\nEnter the number in sample (n): ")) 
    select <- as.numeric(readline(prompt ="\nEnter the number of selection (x) : "))
    chival<-qchisq(alpha,number)
      if(number == "" || select == "")
    {
      cat("\n wrong input entered .. calculator exiting")
      Sys.sleep(2)
      return()
    }
    theta <- select/number
    
    lvalue <- (theta - zval*(theta * (1-theta))^1/2)/(number)^1/2
    hvalue <- (theta + zval*(theta * (1-theta))^1/2)/(number)^1/2
    
    cat("\n The confidence interval is : ",lvalue ," to ",hvalue)
  }
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
#Estimation of Variance  
  

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  eov <- function()
  { 
    number <- readline(prompt = "\nEnter the number of sample : ")
    variance <- readline(prompt = "\n Enter the variance of sample : ")
    chival<-qchisq(alpha,number)
    if(number == "" || variance == "")
    {
      cat ("\nwrong input parameters.. calculator is exiting now")
      Sys.sleep(2)
      return()
    }
    lvalue <- (number-1)*variance/chival(alpha,number-1)
    hvalue <- (number -1)*variance/chival(1-alpha,number-1)
    
    cat("\n the interval is",lavalue," to ",hvalue)
  }
 
  
   
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
#Estimation of diffrence of mean
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  eodm <- function()
  {
    number1 <- as.double(readline(prompt = " Enter the size of popualtion 1 : "))
    mean1 <- as.double(readline(prompt = " Enter the mean of population 1 : "))
    variance1 <- as.double(readline(prompt = " Enter the variance of population 1 :"))
    
    number2 <- as.double(readline(prompt = " Enter the size of population 2 : "))
    mean2 <- as.double(readline(prompt = " Enter the mean of population 2 : "))
    variance2 <- as.double(readline(prompt = " Enter the variance of population 2 : "))
    
    if(is.na(number1) || is.na(number2) || number1 == 0 || number2 == 0)
    {
      cat("\n Oops.. you entered wrong values. Calculator will exit in 2 sec")
      Sys.sleep(2)
      return()
    }
    if(is.na(variance2) || is.na(variance1))
    {
      cat("\n You have not entered variance of either of sample \n please enter variances for both population")
      pvariance1 <- as.double(readline(prompt = " Enter the variance of population 1 :"))
      pvariance2 <- as.double(readline(prompt = " Enter the variance of population 2 :"))
      
      if(is.na(pvariance2) || is.na(pvariance1))-
      {
        cat("\n wrong parameter entered.. calculator will exit in 2 secs")
        Sys.sleep(2)
        return()
      }
      if(number1 > 30 && number2 > 30)
      {
        value <- (number2 * pvariance1) + (number1 * pvariance2)
        value <- value/(number2*number1)
        value <- (value)^0.5
        value <- value * abs(qnorm(alpha))
        lvalue <- mean1 - mean2 - value
        hvalue <- mean1 - mean2 + value
      }
      else
      {
        sdpopulation <- ((number1 - 1)*pvariance1) + ((number2 - 1)*pvariance2)
        sdpopulation <- sdpopulation/(number1+number2-2)
        sdpopulation <- (sdpopulation)^0.5
        value <- number1 + number2
        value <- value / (number2 * number1)
        value <- (value)^0.5
        value <- value * sdpopulation
        value <- value * qt(alpha,number2+number1-2)
        lvalue <- mean1 - mean2 - value
        hvalue <- mean1 - mean2 + value
      }
    }
    else
    {
      value <- (variance1 * number2) + (variance2 * number1)
      value <- value/(number1 * number2)
      value <- (value)^0.5
      value <- value * abs(qnorm(alpha))
      lvalue <- mean1 - mean2 - value
      hvalue <- mean1 - mean2 + value
    }
    
    cat("\n The range is from ",lvalue ," to ",hvalue)
  }
  
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

#Estimation of diffrence of propotion

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
    eodp <- function()
  {
    number1 <- as.double(readline(prompt = " Enter the length of sample 1 : "))
    select1 <- as.double(readline(prompt = " Enter the selection from sample 1 : "))
    number2 <- as.double(readline(prompt = " Enter the length of sample 2 : "))
    select2 <- as.double(readline(prompt = " Enter the selection from sample 2 : "))
    theta1 <- select1/number1
    theta2 <- select2/number2
    value <- (theta1 * (1 - theta1))/number1
    value <- value + ((theta2 * (1 - theta2))/number2)
    value <- (value)^0.5
    value <- value * abs(qnorm(alpha))
    lvalue <- theta1 - theta2 - value
    hvalue <- theta1 - theta2 + value
    
    cat("\n The confidence interval lies between ",lvalue," to ",hvalue)
    
    }
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

#Estimation for diffrence of variances

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
    eodv <- function()
  {
    number1 <- as.double(readline(prompt = " Enter the size of population 1 :"))
    variance1 <- as.double(readline(prompt = " Enter the variance of population 1 : "))
    number2 <- as.double(readline(prompt = " Enter the size of population 2 :"))
    variance2 <- as.double(readline(prompt = " Enter the variance of population 2 : "))
    lvalue <- variance1/(variance2 * qf(alpha,number1-1,number2-1))
    hvalue <- variance1 * qf(alpha,number2-1,number1-1)
    cat("\n The interval is between ",lvalue," to ",hvalue)
  }
 
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Selection Module for category selection 
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
selection_module7<-function()
{ 
  cat("\nSelect your choice : ","\n1. Estimation of means","\n2. Estimation of Differences in means", "\n3. Estimation of propotions","\n4. Estimation of differences in proportions", "\n5. Estimation of Variance","\n6. Estimation of differences in Variances ","\n7. MainModule") 
  choice <- readline(prompt = "Enter your choice : ")
  if(choice==1)
  {
    print("Estimation of mean")
    print(eom())
    return(rerun_mod7())
  }
 else if(choice==2)
  {
    print("Estimation of differences in means")
    print(eodm())
    return(rerun_mod7())
  }
  else if(choice==3)
  {
    print("Estimation of proportions")
    print(eop())
    return(rerun_mod7())
  }
  else if(choice==4)
  {
    print("Estimation of differences in proportions")
    print(eodp())
    return(rerun_mod7())
  }
  else if(choice==5)
  {
    print("Estimation of Variances")
    print(eov())
    return(rerun_mod7())
  }
  else if(choice==6)
  {
    print("Estimation of differences in Variances")
    print(eodv())
    return(rerun_mod7())
  }
  else if(choice==7)
  {
    return(MainModule())
  }
  else
  {
    print("Invalid Selection")
  }
}
return(selection_module7())
} 



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Module8. NON-PARAMETRIC ANALYSIS


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module8<-function()
{
  cinterval<-as.numeric(readline(prompt = "Enter the Confidence interval ( %age) : "))
  alpha<-(100-cinterval)/100
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
#Sign Test
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  signtest<-function()
  {  
    
    Bin<-function(X,n,prob)
    {
      
      if(prob>1 || prob<0)
      {
        print("ERROR in Probability, Please check your inputs")
      }
      else
      { return(Combination(n,X)*(prob^X)*(1-prob)^(n-X))
      }
    }	
    count<-0
    print("Enter the data (Press Enter to Exit read state) :")
    dat<-scan()
    len<-length(dat)
    test<-as.numeric(readline(prompt = "Enter the test condition : "))
    nullhyp<-readline(prompt = "Enter the Null hypothesis you want to test :")
    alternatehyp<-readline(prompt = "Enter the Alternate hypothesis : ")
    if(len<30)
    {
      for(i in 1:length(dat))
      {
        if(dat[i]<test)
        {
          dat[i]<-(-(dat[i]))
          count<-count+1
        }
        else if(dat[i]==test)
        {
          dat[i]<-0
          len<-len-1
        }
      }
      pos<-len-count
      cat("Total number of +ve signs are : ",pos)
      theta<-1/2
      result<-Bin(pos,len,theta)
      cat("\nThe calculated Result is",result)
      if(result<alpha)
      {
        cat("\nNull Hypothesis that ",nullhyp," is rejected ")
      }
      else
      {
        cat("\nNull Hypothesi that ",nullhyp," is accepted")
      }
    }
    else
    {
      for(i in 1:length(dat))
      { 
        if(dat[i]<test)
        {
          dat[i]<-(-(dat[i]))
          count<-count+1
        }
        else if(dat[i]==test)
        {
          dat[i]<-0
          len<-len-1
        }
      }
      pos<-len-count
      cat("Total number of +ve signs are : ",pos)
      theta<-1/2
      X<-as.numeric(readline(prompt = "Enter the mean value : "))
      z<-(X-(len*theta))/sqrt((len*theta)*(1-theta))
      zval<-abs(qnorm(alpha))
      cat("\nCalculated value is : ", z)
      if(z < zval)
      {
        cat("\nNULL hyptothesis that ", nullhyp, " is accepted")
      }
      else
      {
        cat("\nNULL hyptothesis that ", nullhyp, " is rejected")
      }
    }
  }


  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
#WilcoxonTest  

  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
    
  
  WSRT <- function()
  {
    print("Enter the data (Press Enter to Exit read state) :")
    data<-scan()
    mu<-as.numeric(readline(prompt = "Enter the test condition : "))
    nullhyp<-readline(prompt = "Enter the Null hypothesis you want to test :")
    tneg = 0
    tpos = 0
    count0 = 1
    data_minus_mu <- data - mu
    
    data_rank = SortRank(data_minus_mu)  
    
    for(i in 1:length(data_rank)){
      if( data_rank[i] == 0.0)
      {
        count0 = count0 + 1
      }
    }
    size <- length(data_rank)
    data_rank = data_rank[count0:size]
    
    for(i in 1:length(data_rank)){
      
      if(data_rank[i] < 0){
        tneg = tneg+i
      }
      else if (data_rank[i] > 0){
        tpos = tpos+i
      }
    }
    n = length(data_rank)
    t_min = min(tneg,tpos)
    cat("\nSelect your choice ","\n1. Two-Tailed ", "\n2. Right-tailed ", "\n3. Left-Tailed")
    case<-readline(prompt = "Enter your choice : ")
    if(case == 1)
    { 
      tvalue = qsignrank(alpha/2,n)
      
      if(t_min <= tvalue)
      {
        cat("Null Hypothesis that",nullhyp, "is rejected")
      }
      else{
        cat("Null Hypothesis that",nullhyp, "is accepted")      
      }
    }
    else if(case == 2)
    {
      tvalue = qsignrank(alpha,n)
      
      if(tneg <= tvalue)
      {
        cat("Null Hypothesis that",nullhyp, "is rejected")
      }
      else{
        cat("Null Hypothesis that",nullhyp, "is accepted")
        
      }
    }
    else if(case == 3){
      tvalue = qsignrank(alpha,n)
      
      if(tpos <= tvalue)
      {
        
        cat("Null Hypothesis that",nullhyp, "is rejected")
      }
      else{
        cat("Null Hypothesis that",nullhyp, "is accepted")
        
      }
    }
    
  }  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

#Mann-Whitney Test
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

    MWT<- function(){
    
    print("Enter the data1 (Press Enter to Exit read state) :")
    data1<-scan()
    print("Enter the data2 (Press Enter to Exit read state) :")
    data2<-scan()
    nullhyp<-readline(prompt = "Enter the Null hypothesis you want to test :")
    alternatehyp<-readline(prompt = "Enter the Alternate hypothesis : ")
    
    total_data = as.numeric()
    
    for(i in 1:length(data1)){
      total_data = c(total_data,d1=data1[i])
    }
    for(i in 1:length(data2)){
      total_data = c(total_data,d2=data2[i])
    }
    
    total_data = UserSortKruskal(total_data)
    total_data_rank= c(1)
    
    for(i in 2:length(total_data)){
      if( total_data[i] == total_data[i-1] )
      {
        avg = (i+i-1)/2
        total_data_rank[i-1]=avg
        total_data_rank[i]=avg
      }
      else
      {
        total_data_rank[i]=i
      }
    }
    W1 <-0
    W2 <-0
    
    for(i in 1:length(total_data_rank))
    {
      if(names(total_data[i]) == "d1")
      {
        W1 <- W1 + total_data_rank[i]
      }
      else if(names(total_data[i]) == "d2")
      {
        W2 <- W2 + total_data_rank[i]
      }
    }
    n1 = length(data1)
    n2 = length(data2)
    U1 = W1 - (n1*(n1+1))/2
    U2 = W2 - (n2*(n2+1))/2
    U_cal = min(U1,U2)
    cat("\nSelect your choice ","\n1. Two-Tailed ", "\n2. Right-tailed ", "\n3. Left-Tailed")
    case<-readline(prompt = "Enter your choice : ")
    if(case == 1)
    {
      U = qwilcox(alpha/2,n1,n2)-1
      if(U_cal <= U)
      {
        cat("\nNULL hyptothesis that ", nullhyp, " is rejected")
      }
      else{
        cat("\nNULL hyptothesis that ", nullhyp, " is accepted")
      }
    }
    else if(case == 2)
    {
      U = qwilcox(alpha,n1,n2)-1
      if(U2 <= U)
      {
        cat("\nNULL hyptothesis that ", nullhyp, " is rejected")
      }
      
      else{
        cat("\nNULL hyptothesis that ", nullhyp, " is accepted")
      }  
    }
    else if( case == 3)
    {
      U = qwilcox(alpha,n1,n2)-1
      if(U1 <= U)
      {
        cat("\nNULL hyptothesis that ", nullhyp, " is rejected")
      }
      else{
        cat("\nNULL hyptothesis that ", nullhyp, " is accepted")
      }
    }
    }
    
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
#Mann-Whitney Test
    
    
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
 KWT <- function()
    {
      print("Enter the data1 (Press Enter to Exit read state) :")
      data1<-scan()
      print("Enter the data2 (Press Enter to Exit read state) :")
      data2<-scan()
      data3<-0
      nullhyp<-readline(prompt = "Enter the Null hypothesis you want to test :")
      alternatehyp<-readline(prompt = "Enter the Alternate hypothesis : ")
      total_data = as.numeric()
      
      for(i in 1:length(data1)){
        total_data = c(total_data,d1=data1[i])
      }
      for(i in 1:length(data2)){
        total_data = c(total_data,d2=data2[i])
      }
      for(i in 1:length(data3)){
        total_data = c(total_data,d3=data3[i])
      }
      
      total_data = UserSortKruskal(total_data)
      total_data_rank= c(1)
      
      for(i in 2:length(total_data)){
        if( total_data[i] == total_data[i-1] )
        {
          avg = (i+i-1)/2
          total_data_rank[i-1]=avg
          total_data_rank[i]=avg
        }
        else
        {
          total_data_rank[i]=i
        }
      }
      R1 <-0
      R2 <-0
      R3 <-0
      
      for(i in 1:length(total_data_rank))
      {
        if(names(total_data[i]) == "d1")
        {
          R1 <- R1 + total_data_rank[i]
        }
        else if(names(total_data[i]) == "d2")
        {
          R2 <- R2 + total_data_rank[i]
        }
        else if(names(total_data[i]) == "d3")
        {
          R3 <- R3 + total_data_rank[i]
        }
        
      }
      R1 <- (R1*R1)/length(data1)
      R2 <- (R2*R2)/length(data2)
      R3 <- (R3*R3)/length(data3)
      n = length(total_data)
      H_calculated = (12/(n*(n+1)))*(R1+R2+R3) - 3*(n+1)
      H_observed = qchisq(1-alpha,n-1)
      if(H_calculated > H_observed)
      {
        cat("\nNULL hyptothesis that ", nullhyp, " is rejected")
      }
      else{
        cat("\nNULL hyptothesis that ", nullhyp, " is accepted")
      }
    }
    
    
        
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

#SELECTION MODULE FOR MODULE8  
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  selection_module8<-function()
  { 
    cat("\nSelect your choice : ","\n1. Sign Test", "\n2. Wilcoxon Signed Rank Test", "\n3. Mann-Whitney Test","\n4. Kruskal-Wallis Test","\n5. Main Module") 
    choice <- readline(prompt = "Enter your choice : ")
    if(choice==1)
    {
      print("SIGN TEST")
      print(signtest())
      return(rerun_mod8())
    }
    else if(choice==2)
    {
      print("WILCOXON SIGNED RANK TEST")
      print(WSRT())
      return(rerun_mod8())
    }
    else if(choice==3)
    {
      print("MANN-WHITNEY TEST")
      print(MWT())
      return(rerun_mod8())
    }
    else if(choice == 4)
    {
      print("KRUSKAL-WALLIS TEST")
      print(KWT())
      return(rerun_mod8())
    }
    else if(choice==5)
    {
      return(MainModule())
    }
    else
    {
      print("Invalid Selection")
    }
  }
  return(selection_module8())
  
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Module9. VISUALISATION


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


Module9<- function()
{
  cat("\n In this module, inbuilt datasets are used to show the various visualizations")
  cat("\n Each graphs will disappears in 5 seconds")
  cat("\n 1). Histogram (dataset used is volcano")
  hist(volcano,density = 20,col = rainbow(10),border = "black",angle = 90,labels = TRUE,ylim = c(0,1500))
  Sys.sleep(5)
  cat("\n 2). Line Graph (dataset used is JohnsonJohnson)")
  graph <- as.matrix(JohnsonJohnson)
  plot(graph,xlim = c(30,100))
  Sys.sleep(1)
  lines(graph)
  Sys.sleep(5)
  cat("\n 3). bar Graph (dataset used is Cars")
  barplot(cars$speed,cars$dist,col = rainbow(7),density = 80,xlab = "distance",ylab = "speed",border = "black")
  Sys.sleep(5)
  cat("\n 4). Pie chart (dataset used is trees) ")
  pie(trees$Height,col = rainbow(6),density = 100,radius = 1,clockwise = TRUE,border = "black")
  Sys.sleep(5)
  cat("\n 5). scatter plot (dataset used is swiss) ")
  education <- swiss$Education
  examination <- swiss$Examination
  plot(education,examination,xlim = c(1,40))
  Sys.sleep(5)
  cat("\n 6). Box plot (dataset used is swiss)")
  plot(education,examination,xlim = c(1,40))
  Sys.sleep(5)
  box(lty = "solid",col = "green")
  cat("\n 7). Qunatile Quantile plot (dataset used is faithful)")
  qqplot(faithful$eruptions,faithful$waiting,xlim=c(1.5,3.0),xlab = "Eruptions",ylab = "Waiting")
  Sys.sleep(5)  
  cat("\n 8). stem-leaf plot (dataset used is iris3) ")
  stem(iris3)
  
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
#CATEGORY SELECTION FOR MODULE9.
  
  
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  selection_module9<-function()
  {
    cat("\n1. Visualize the data again ? ", "\n2. Main Module")
    choice<-as.numeric(readline(prompt = "Enter your choice : "))
    if(choice==1)
    {
      return(rerun_mod9())
    }
    else if(choice==2)
    {
      return(MainModule())
      
    }
    else
    {
      print("Invalid Selection")
    }
  }
  selection_module9()
}



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------



#MODULE SELECTION



#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

MainModule<-function()
{ 
  cat("MODULE1. DESCRIPTIVE ANALYSIS","\nMODULE2. PREDICTIVE ANALYSIS","\nMODULE3. PROBABILITY ANALYSIS","\nMODULE4. DISCRETE DISTRIBUTION FUNCTIONS","\nMODULE5. CONTINUOUS DISTRIBUTION FUNCTIONS","\nMODULE6. SAMPLE TEST STATISTICS","\nMODULE7. INTERVAL ESTIMATION","\nMODULE8. NON-PARAMETRIC ANALYSIS","\nMODULE9. VISUALIZATION")
  choice<-readline(prompt = "Enter the Choice :- ")
  if(choice==1)
  {
    print("DESCRIPTIVE ANALYSIS")
    return(Module1())
  }
  else if(choice==2)
  {
    print("PREDICTIVE ANALYSIS")
    return(Module2())
  }
  else if(choice==3)
  {
    print("PROBABILITY ANALYSIS")
    return(Module3())
    
    
  }
  else if(choice==4)
  {
    
    print("DISCRETE DISTRIBUTION FUNCTIONS")
    return(Module4())
    
  }
  else if(choice==5)
  {
    print("CONTINUOUS DISTRIBUTION FUNCTION")
    return(Module5())
  
  }
  else if(choice==6)
  {
    return(Module6())
  }
  
  else if(choice==7)
  { 
    print("INTERVAL ESTIMATION")
    return(Module7())
  
  }
  
  else if(choice==8)
  { 
    print("NON-PARAMETRIC ANALYSIS")
    return(Module8())
    
  }
  else if(choice==9)
  {
    print("VISUALIZATION")
    return(Module9())
  }
  else
  {
    print("Invalid Selection, EXITING!!!")
  }
  
}

MainModule()




