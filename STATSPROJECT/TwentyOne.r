#------------------------------------------------------------------------------------------------------------------
#-------------------------------------CREATED BY :- Jayant & Vishal------------------------------------------------
#-------------------------------------ROLL NUMBER :- 16 & 41-------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
#---------------------------------------------GAME OF 21-----------------------------------------------------------
#Rules :- 1.) if your score is less than 21 but greater than the computer, YOU WIN else COMPUTER WINS
#         2.) if your score exceeds 21 , YOU LOSE
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------
deck <- c("spade","club","diamond","heart")
cards <- c("Ace",2,3,4,5,6,7,8,9,10,"Jack","Queen","King")
cards_value<-c(1,2,3,4,5,6,7,8,9,10,10,10,10)
#------------------------------------------------------------------------------------------------------------------

#UserScore = User's sum or score
#ComputerScore = Computer's sum or score

#------------------------------------------------------------------------------------------------------------------


UserScore<- 0
ComputerScore <- 0

#------------------------------------------------------------------------------------------------------------------

#The game function TwentyOne()

#------------------------------------------------------------------------------------------------------------------

TwentyOne <- function()
{
  
  print("************************** USER'S TURN *********************************")

  deck_r <- sample(1:4,1)
  card <- sample(1:13,1)
  cat("YOUR CARD IS ",cards[card]," OF ",deck[deck_r],"\n")
  if (card==11 || card==12 || card==13)
  {
    UserScore <- UserScore + 10
  }
  else
  {
    UserScore <- UserScore + cards_value[card]
  }
#------------------------------------------------------------------------------------------------------------------
    userturn <- function()
  {
    deck_r <- sample(1:4,1)
    card2 <- sample(1:13,1)
    cat("YOUR CARD IS ",cards[card2]," OF ",deck[deck_r],"\n")
    if ((card2 == 1) && (UserScore + 11 > 21))
    {
      UserScore <<- UserScore + 1
      print(UserScore)
    }
    if ((card2 == 1) && (UserScore + 11 < 21))
    { 
      UserScore <<- UserScore+10
    }
    if ((card2 == 11) || (card2 == 12) || (card2 == 13))
    {
      UserScore <<- UserScore + 10
    }
    
    else
    {
      UserScore <<- UserScore + cards_value[card2]
    }   
    
    cat(" Score is",UserScore,"\n")  
    
    if (UserScore > 21)
    {
      return()
    }
    
    else
    {
      repeat
      {
        choice <- readline(prompt = "Do you want to continue (HIT/STAY) : ")
        if ((choice == "HIT") || (choice == "hit") || (choice == "stay") || (choice == "STAY"))
          break
      }
      
      if ((choice == "HIT") || (choice == "hit"))
      {
        return(userturn())
      }
    }
  }
  userturn()
  if(UserScore <= 21)
  {
    cat("User's Final Score is",UserScore,"\n")
  }
  if(UserScore > 21)
  {
    print("your sum exceeds 21 \n")
    print("You Loose \n")
    nextgame()
    return()
  }
  if(UserScore == 21)
  {
    print("Congratulations! \n")
    print("You Won \n")
    nextgame()
    return()
  }

  cat("\n","\n","Shuffled Deck","\n","\n")

  print("************************** COMPUTER'S TURN *********************************")



#-----------------------------------------------------------------------------------------------------------------

#---------------------------------------COMPUTER'S TURN-----------------------------------------------------------  
  
#-----------------------------------------------------------------------------------------------------------------  
  
    computerturn <- function() 
  {
    deck_r <- sample(1:4,1)
    card <- sample(1:13,1)
    cat("Computer got",cards[card]," of ",deck[deck_r],"\n")
    if((card == 1) && (ComputerScore + 11 > 21))
    {
      ComputerScore <<- ComputerScore + 1
    }
    if((card == 1) && (UserScore + 11 < 21))
    {
      ComputerScore <<- ComputerScore + 10
    }
    if((card == 11) || (card == 12) || (card == 13))
    {
      ComputerScore <<- ComputerScore + 10
    }
    else
    {
      ComputerScore <<- ComputerScore + card
    }

    cat("Computer's score is",ComputerScore,"\n")
    
    if(ComputerScore > 17)
    {
      return()
    }
    else
    {
      return(computerturn())
      }
    
  }
  computerturn()
  if(ComputerScore < 21)
  {
    cat("Final score of Computer is",ComputerScore,"\n")
  }
  if(ComputerScore > 21)
  {
    cat("Sum exceeds 21","\n")
    cat("User Won","\n")
    nextgame()
    return()
  }
  if(ComputerScore == 21)
   {
     print("Computer Wins","\n")
     nextgame()
     return()
    }
  if(ComputerScore < 21 && UserScore < 21 && ComputerScore > UserScore)
    {
      print("Computer wins the game","\n")
      nextgame()
      return()
    }
  if(ComputerScore < 21 && UserScore < 21 && ComputerScore < UserScore)
    {
      print("User wins the game","\n")
      nextgame()
      return()
    }
}

#-----------------------------------------------------------------------------------------------------------------

#---------------------------ASKING USER TO PLAY GAME OF TWENTYONE AGAIN-------------------------------------------

#-----------------------------------------------------------------------------------------------------------------

nextgame <- function()
{
  ComputerScore <<- 0
  UserScore <<- 0
  repeat
  {
    playmore <- readline(prompt = "Do you want to play more (y/n) :")
    if((playmore == "y") || (playmore == "Y") || (playmore =="n") || (playmore == "N"))
      break
  }
  if((playmore == "y") || (playmore == "Y"))
    return(TwentyOne())
  if((playmore =="n") || (playmore == "N"))
  {print("Thank you for playing game")
    returnValue(0)}
}
TwentyOne()