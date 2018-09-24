KL = c("Kyle Lowry",0.03,0.161,0,0,963,169,82,52,52,608,0,0.657,0.293,0.327,0.346,0.395,0.138-0.138/4,0.854,0.309,0.267,0.643)
DD = c("DeMar DeRozan",0.025,0.104,0,0,1452,287,247,349,271,298,31,0.672,0.466,0.441,0.402,0.315,0.198-0.198/4,0.825,0.25,0.263,0.674)
OG = c("OG Anunoby",0.034,0.104,0,0,358,128,17,5,5,203,49,0.695,0.235,0.2,0,0.374,0.0895-0.0895/4,0.629,0.05,0.711,0.528)
SI = c("Serge Ibaka",0.043,0.21,0,0,806,129,127,124,122,304,47,0.736,0.480,0.508,0.5,0.365,0.0755-0.0755/4,0.797,0.044,0.699,0.953)
JV = c("Jonas Valanciunas",0.121,0.303,0,0,705,310,237,65,17,76,75,0.71,0.477,0.4,0.706,0.408,0.01535-0.01535/4,0.806,0.075,0.636,0.9)
raps = matrix(c(KL,DD,OG,SI,JV),22,5)
row.names(raps) <- c("Name","oreb%","dreb%","oreb","dreb","FGA","rim","3to10","10to16","16to3","3","dunk","rimc","3to10c","10to16c","16to3c","3c","FTr","FTc","ast%","2ast","3ast")

KLbox = c("Kyle Lowry",0,0,0,0,0,0,0,0,0,0,0)
DDbox = c("DeMar DeRozan",0,0,0,0,0,0,0,0,0,0,0)
OGbox = c("OG Anunoby",0,0,0,0,0,0,0,0,0,0,0)
SIbox = c("Serge Ibaka",0,0,0,0,0,0,0,0,0,0,0)
JVbox = c("Jonas Valanciunas",0,0,0,0,0,0,0,0,0,0,0)
rapsBox = matrix(c(KLbox,DDbox,OGbox,SIbox,JVbox),12,5)
row.names(rapsBox) <- c("Name","FG","FGA","3FG","3FGA","FT","FTA","PTS","REB","DREB","OREB","AST")

CP = c("Chris Paul",0.023,0.167,0,0,830,79,91,182,88,390,0,0.671,0.451,0.5,0.557,0.379,0.1385-0.1385/4,0.919,0.409,0.081,0.243)
JH = c("James Harden",0.018,0.152,0,0,1496,408,176,106,58,744,20,0.654,0.347,0.434,0.362,0.367,0.251-0.251/4,0.858,0.451,0.088,0.26)
LM = c("Luc Mbah a Moute",0.016,0.117,0,0,345,139,26,0,7,173,25,0.676,0.346,0,0,0.364,0.1375-0.1375/4,0.684,0.05,0.777,0.984)
TA = c("Trevor Ariza",0.017,0.129,0,0,671,149,34,5,7,476,20,0.631,0.176,0,0.143,0.363,0.0685-0.0685/4,0.854,0.068,0.592,0.953)
CC = c("Clint Capela",0.135,0.308,0,0,697,561,123,12,0,1,244,0.717,0.382,0.5,0,0,0.19-0.19/4,0.56,0.057,0.819,0)
rockets = matrix(c(CP,JH,LM,TA,CC),22,5)
row.names(rockets) <- c("Name","oreb%","dreb%","oreb","dreb","FGA","rim","3to10","10to16","16to3","3","dunk","rimc","3to10c","10to16c","16to3c","3c","FTr","FTc","ast%","2ast","3ast")

CPbox = c("Chris Paul",0,0,0,0,0,0,0,0,0,0,0)
JHbox = c("James Harden",0,0,0,0,0,0,0,0,0,0,0)
LMbox = c("Luc Mbah a Moute",0,0,0,0,0,0,0,0,0,0,0)
TAbox = c("Trevor Ariza",0,0,0,0,0,0,0,0,0,0,0)
CCbox = c("Clint Capela",0,0,0,0,0,0,0,0,0,0,0)
rocketsBox = matrix(c(CPbox,JHbox,LMbox,TAbox,CCbox),12,5)
row.names(rocketsBox) <- c("Name","FG","FGA","3FG","3FGA","FT","FTA","PTS","REB","DREB","OREB","AST")

#SC = c("StephenCurry",0.027,0.144,0,0,1300,)
#KT = c("Klay Thompson")
#KD = c("Kevin Durant")
#DG = c("Draymond Green")
#ZP = c("Zaza Pachulia")
#warriors = matrix(c(SC,KT,KD,DG,ZP),22,5)
#row.names(warriors) <- c("Name","oreb%","dreb%","oreb","dreb","FGA","rim","3to10","10to16","16to3","3","dunk","rimc","3to10c","10to16c","16to3c","3c","FTr","FTc","ast%","2ast","3ast")

#SCbox = c("Stephen Curry",0,0,0,0,0,0,0,0,0,0,0)
#KTbox = c("Klay Thompson",0,0,0,0,0,0,0,0,0,0,0)
#KDbox = c("Kevin Durant",0,0,0,0,0,0,0,0,0,0,0)
#DGbox = c("Draymond Green",0,0,0,0,0,0,0,0,0,0,0)
#ZPbox = c("Zaza Pachulia",0,0,0,0,0,0,0,0,0,0,0)
#warriorsBox = matrix(c(SCbox,KTbox,KDbox,DGbox,ZPbox),12,5)
#row.names(rapsBox) <- c("Name","FG","FGA","3FG","3FGA","FT","FTA","PTS","REB","DREB","OREB","AST")
#League Wide and1% = 0.263



gametime <- function(a,b,aBox,bBox) {
  # Team a starts
  time = 720*3
  #ShotMaker = 1
  #orebMod = 2
  #orebCatcher = 3
  #orebProb = 4
  #teamA = 5
  #teamB = 6
  teamA = 0
  teamB = 0
  Mod = c(0,0,0,1,0)
  
  
  while (time > 0) {
    cycle = 0
    t = shotClock(Mod[2])
    if (time - t < 0) {
      t = time
    }
    cat(integerToTime(time - t)," ")
    time = time - t
    if (Mod[1] == 0) {
      #cat(Mod,"   ")
      shoota = shooter(a,Mod)
      shot = shoot(a[1:22,shoota],Mod)
      teamA = teamA + shot[2]
      if (shot[1] == TRUE) {
        Mod = c(1,0,0,1,teamA,teamB)
        cat(" (",teamA,"-",teamB,")\n",sep="")
        if (shot[2] == 2) {
          ast = assisting(a,a[1:22,shoota],2)
          if (ast != 0 && shot[3] != 1) {
            aBox[12,ast] = as.numeric(aBox[12,ast]) + 1
          }
        }
        else if (shot[2] == 3 && shot[3] != 1) {
          aBox[4,shoota] = as.numeric(aBox[4,shoota]) + 1
          aBox[5,shoota] = as.numeric(aBox[5,shoota]) + 1
          ast = assisting(a,a[1:22,shoota],3)
          if (ast != 0) {
            aBox[12,ast] = as.numeric(aBox[12,ast]) + 1
          }
        }
        if (as.numeric(shot[3]) == 1) {
          aBox[6,shoota] = as.numeric(aBox[6,shoota]) + as.numeric(shot[2])
          aBox[7,shoota] = as.numeric(aBox[7,shoota]) + 2
          aBox[8,shoota] = as.numeric(aBox[8,shoota]) + as.numeric(shot[2])
        } 
        else {
        
          aBox[8,shoota] = as.numeric(aBox[8,shoota]) + as.numeric(shot[2])
          aBox[2,shoota] = as.numeric(aBox[2,shoota]) + 1
          aBox[3,shoota] = as.numeric(aBox[3,shoota]) + 1
        
        }
      } else {
        if (as.numeric(shot[3]) == 1) {
          aBox[6,shoota] = as.numeric(aBox[6,shoota]) + as.numeric(shot[2])
          aBox[7,shoota] = as.numeric(aBox[7,shoota]) + 2
          aBox[8,shoota] = as.numeric(aBox[8,shoota]) + as.numeric(shot[2])
        } 
        if (shot[3] == 3) {
          aBox[5,shoota] = as.numeric(aBox[5,shoota]) + 1
        }
        aBox[3,shoota] = as.numeric(aBox[3,shoota]) + 1
        cat(integerToTime(time - 1)," ")
        time = time - 1
        reb = rebounding(a,b)
        if (reb[2] == "dreb") {
          bBox[10,as.numeric(reb[1])] = as.numeric(bBox[10,as.numeric(reb[1])]) + 1
          bBox[9,as.numeric(reb[1])] = as.numeric(bBox[9,as.numeric(reb[1])]) + 1
          Mod = c(1,0,0,1,teamA,teamB)
        } else {
          aBox[11,as.numeric(reb[1])] = as.numeric(aBox[11,as.numeric(reb[1])]) + 1
          aBox[9,as.numeric(reb[1])] = as.numeric(aBox[9,as.numeric(reb[1])]) + 1
          Mod = c(0,1,as.numeric(reb[1]),runif(1),teamA,teamB)
        }
      }
      cycle = 1
    }
    if (Mod[1] == 1 && cycle == 0) {
      #cat(Mod,"   ")
      shoota = shooter(b,Mod)
      shot = shoot(b[1:22,shoota],Mod)
      teamB = teamB + shot[2]
      if (shot[1] == TRUE) {
        Mod = c(0,0,0,1,teamA,teamB)
        cat(" (",teamA,"-",teamB,")\n",sep="")
        if (shot[2] == 2) {
          ast = assisting(b,b[1:22,shoota],2)
          if (ast != 0 && shot[3] != 1) {
            bBox[12,ast] = as.numeric(bBox[12,ast]) + 1
          }
        }
        else if (shot[2] == 3) {
          bBox[4,shoota] = as.numeric(bBox[4,shoota]) + 1
          bBox[5,shoota] = as.numeric(bBox[5,shoota]) + 1
          ast = assisting(b,b[1:22,shoota],3)
          if (ast != 0 && shot[3] != 1) {
            bBox[12,ast] = as.numeric(bBox[12,ast]) + 1
          }
        }
        if (as.numeric(shot[3]) == 1) {
          bBox[6,shoota] = as.numeric(bBox[6,shoota]) + as.numeric(shot[2])
          bBox[7,shoota] = as.numeric(bBox[7,shoota]) + 2
          bBox[8,shoota] = as.numeric(bBox[8,shoota]) + as.numeric(shot[2])
        } 
        else
        {
          bBox[8,shoota] = as.numeric(bBox[8,shoota]) + as.numeric(shot[2])
          bBox[2,shoota] = as.numeric(bBox[2,shoota]) + 1
          bBox[3,shoota] = as.numeric(bBox[3,shoota]) + 1
        }
      } else {
        if (as.numeric(shot[3]) == 1) {
          bBox[6,shoota] = as.numeric(bBox[6,shoota]) + as.numeric(shot[2])
          bBox[7,shoota] = as.numeric(bBox[7,shoota]) + 2
          bBox[8,shoota] = as.numeric(bBox[8,shoota]) + as.numeric(shot[2])
        } 
        if (shot[3] == 3) {
          bBox[5,shoota] = as.numeric(bBox[5,shoota]) + 1
        }
        bBox[3,shoota] = as.numeric(bBox[3,shoota]) + 1
        if (time == 0) {
          cat(integerToTime(time)," ")
        } else {      
          cat(integerToTime(time - 1)," ")
        }
        time = time - 1
        reb = rebounding(b,a)
        if (reb[2] == "dreb") {
          aBox[10,as.numeric(reb[1])] = as.numeric(aBox[10,as.numeric(reb[1])]) + 1
          aBox[9,as.numeric(reb[1])] = as.numeric(aBox[9,as.numeric(reb[1])]) + 1
          Mod = c(0,0,0,1,teamA,teamB)
        } else {
          bBox[11,as.numeric(reb[1])] = as.numeric(bBox[11,as.numeric(reb[1])]) + 1
          bBox[9,as.numeric(reb[1])] = as.numeric(bBox[9,as.numeric(reb[1])]) + 1
          Mod = c(1,1,as.numeric(reb[1]),runif(1),teamA,teamB)
        }
      }
    }
  }
  print(aBox);
  print(bBox);
  if (teamA > teamB) {
    cat("Raptors",teamA,"-",teamB,"Rockets")
  } else {
    cat("Rockets",teamB,"-",teamA,"Raptors")
  }
}


## Generates a random shotclock time
shotClock = function(mod) {
  ## Generates a random variable from 0 to 24 to represent when shot is taking place.
  if (mod == 0) {
    if (runif(1) < 0.15) {
      return(ceiling(8*runif(1)))
    } else {
    
    }
    return(ceiling(16*runif(1)) + 8)
  }
  ## In the case of an offensive rebound, generates a time more accurate to a OREB
  else {
    if (runif(1) < 0.66) {
      return(ceiling(3*runif(1)))
    } else {
      return(ceiling(11*runif(1)) + 3)
    }
  }
}

## Converts integer to a time based display.
integerToTime <- function(time) {
  Mins = time %/% 60
  Secs = time %% 60
  
  if (Mins == 0) {
    StrMins = "00"
  } else if (Mins < 10) {
    StrMins = paste("0",toString(Mins),sep="")
  } else {
    StrMins = toString(Mins)
  }
  
  if (Secs == 0) {
    StrSecs = "00"
  } else if (Secs < 10) {
    StrSecs = paste("0",toString(Secs),sep="")
  } else {
    StrSecs = toString(Secs)
  }
  
  StrTime = paste(StrMins, ":", StrSecs, sep="")
  return(StrTime)
}

nameWhiteSpace <- function(Player) {
  addWhite = 20 - nchar(Player["Name"])
  i = 0
  while (i < addWhite) {
    cat(" ")
    i = i + 1
  }
  return(addWhite)
}

shooter <- function(Team,Mod) {
  if (Mod[3] != 0) {
    if (runif(1) < 0.7) {
      return(Mod[3])
    }
  }
  FGattempted = sum(as.numeric(Team["FGA",1:5]))
  x = runif(1)
  for (i in 1:5) {
    if (x < sum(as.numeric(Team["FGA",1:i]))/FGattempted) {
      return(i)
    }
  }
}

and1Chance <- function(Dist) {
  if (Dist < 5) {
    return((1/800)*((Dist-5)^2)+0.03)
  } else if (Dist < 18) {
    return((-3/2500)*(Dist-30))
  } else if (Dist < 30) {
    return((1/33000)*((Dist-30)^2)+0.01)
  } else {
    return(0.001)
  }
}

freeThrows <- function(Player,Dist) {
    firstFreeThrow = 0
    y = runif(1)
    cat(Player[1], "draws a foul on a", Dist, "foot field goal.\n")
    if (runif(1) < as.numeric(Player["FTc"])) {
      cat("      ", Player[1], "makes free throw", 1, "of 2.")
      nameWhiteSpace(Player)
      firstFreeThrow = 1
      cat("                Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4))
      if (Mod[1] == 0) {
        cat(" (",Mod[5]+1,"-",Mod[6],")\n",sep="")
      } else {
        cat(" (",Mod[5],"-",Mod[6]+1,")\n",sep="")
      }
    } else {
      cat("      ", Player[1], "misses free throw", 1, "of 2.")
      nameWhiteSpace(Player)
      cat("               Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4),"\n")
    }
    if (runif(1) < as.numeric(Player["FTc"])) {
      cat("      ", Player[1], "makes free throw", 2, "of 2.")
      nameWhiteSpace(Player)
      cat("                Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4))
      return(c(TRUE,1+firstFreeThrow))
    } else {
      cat("      ", Player[1], "misses free throw", 2, "of 2.")
      nameWhiteSpace(Player)
      cat("               Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4),"\n")
      return(c(FALSE,0+firstFreeThrow))
    }
}

shoot <- function(Player,Mod) {
  NameChange = 20 - nchar(Player["Name"])
  x = runif(1)
  
  if (Mod[2] == 1) {
    if (runif(1) < Mod[4]) {
      while (x > as.numeric(Player["rim"])/as.numeric(Player["FGA"])) {
        x = runif(1)
      }
    }
  }
  
  
  z = runif(1)
  if (z < as.numeric(Player["FTr"])/(1+as.numeric(Player["FTr"]))) {
    firstFreeThrow = 0
    y = runif(1)
    ShotDistance = floor(3*y)
    cat(Player[1], "draws a foul on a", ShotDistance, "foot field goal.\n")
    if (runif(1) < as.numeric(Player["FTc"])) {
      cat("      ", Player[1], "makes free throw", 1, "of 2.")
      nameWhiteSpace(Player)
      firstFreeThrow = 1
      cat("                Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4))
      if (Mod[1] == 0) {
      cat(" (",Mod[5]+1,"-",Mod[6],")\n",sep="")
      } else {
        cat(" (",Mod[5],"-",Mod[6]+1,")\n",sep="")
      }
    } else {
      cat("      ", Player[1], "misses free throw", 1, "of 2.")
    nameWhiteSpace(Player)
      cat("               Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4),"\n")
    }
    if (runif(1) < as.numeric(Player["FTc"])) {
      cat("      ", Player[1], "makes free throw", 2, "of 2.")
      nameWhiteSpace(Player)
      cat("                Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4))
      return(c(TRUE,1+firstFreeThrow,1))
    } else {
      cat("      ", Player[1], "misses free throw", 2, "of 2.")
      nameWhiteSpace(Player)
      cat("               Likelihood =", format(round(as.numeric(Player["FTc"]), 4), nsmall=4),"\n")
      return(c(FALSE,0+firstFreeThrow,1))
    }

  } 
  else if (x < as.numeric(Player["rim"])/as.numeric(Player["FGA"])) {
    y = runif(1)
    ShotDistance = floor(3*y)
    TrueShot = 3*y
    
    Lowrange = (as.numeric(Player["rimc"]) + as.numeric(Player["3to10c"]))/2
    Toprange = as.numeric(Player["rimc"])+abs(as.numeric(Player["rimc"])-Lowrange)
    Aval = 9/(Toprange - Lowrange)
    ShotPercent = (1/Aval)*(TrueShot-3)^2+Lowrange
   
    rand0 = runif(1) 
    if (runif(1) < ShotPercent) {
      if (y < as.numeric(Player["dunk"])/as.numeric(Player["rim"])) {
        cat(Player[1], "makes a", ShotDistance, "foot dunk.")
        nameWhiteSpace(Player)
        cat("                    Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      } else if (rand0 < 0.05) {
        cat(Player[1], "makes a", ShotDistance, "foot field goal.")
        nameWhiteSpace(Player)
        cat("              Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      } else {
        cat(Player[1], "makes a", ShotDistance, "foot layup.")
        nameWhiteSpace(Player)
        cat("                   Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      }
      return(c(TRUE,2,0))
    } else {
      if (y < as.numeric(Player["dunk"])/as.numeric(Player["rim"])) {
        cat(Player[1], "misses a", ShotDistance, "foot dunk.")
        nameWhiteSpace(Player)
        cat("                   Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      } else if (rand0 < 0.05) {
        cat(Player[1], "misses a", ShotDistance, "foot field goal.")
        nameWhiteSpace(Player)
        cat("             Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      } else {
        cat(Player[1], "misses a", ShotDistance, "foot layup.")
        nameWhiteSpace(Player)
        cat("                  Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      }
      return(c(FALSE,0,0))
    }
  }
  else if (x < sum(as.numeric(Player[7:8]))/as.numeric(Player["FGA"])) {
    y = runif(1)
    ShotDistance = floor(7*y)+3
    TrueShot = 7*y+3
    
    Toprange = (as.numeric(Player["rimc"]) + as.numeric(Player["3to10c"]))/2
    Lowrange = (as.numeric(Player["3to10c"]) + as.numeric(Player["10to16c"]))/2
    Aval = 49/(Toprange - Lowrange)
    ShotPercent = (1/Aval)*(TrueShot-10)^2+Lowrange
    
    rand0 = runif(1)
    if (runif(1) < ShotPercent) {
      if ((TrueShot < 4.5) && (1.5*rand0 < TrueShot - 3)) {
        cat(Player[1], "makes a", ShotDistance, "foot layup.")
        nameWhiteSpace(Player)
        cat("                   Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      } else {
        cat(Player[1], "makes a", ShotDistance, "foot field goal.")
        nameWhiteSpace(Player)
        cat("              Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      }
      return(c(TRUE,2,0))
    } else {
      if ((TrueShot < 4.5) && (1.5*rand0 < TrueShot - 3)) {
        cat(Player[1], "misses a", ShotDistance, "foot layup.")
        nameWhiteSpace(Player)
        cat("                  Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      } else {
        cat(Player[1], "misses a", ShotDistance, "foot field goal.")
        nameWhiteSpace(Player)
        cat("             Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      }
      return(c(FALSE,0,0))
    }
  }
  else if (x < sum(as.numeric(Player[7:9]))/as.numeric(Player["FGA"])) {
    y = runif(1)
    ShotDistance = floor(6*y)+10
    TrueShot = 6*y+3
    
    Toprange = (as.numeric(Player["3to10c"]) + as.numeric(Player["10to16c"]))/2
    Lowrange = (as.numeric(Player["10to16c"]) + as.numeric(Player["16to3c"]))/2
    Aval = 36/(Toprange - Lowrange)
    ShotPercent = (1/Aval)*(TrueShot-16)^2+Lowrange
    
    if (runif(1) < ShotPercent) {
      cat(Player[1], "makes a", ShotDistance, "foot field goal.")
      nameWhiteSpace(Player)
      cat("             Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      return(c(TRUE,2,0))
    } else {
      cat(Player[1], "misses a", ShotDistance, "foot field goal.")
      nameWhiteSpace(Player)
      cat("            Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      return(c(FALSE,0,0))
    }
  }
  else if (x < sum(as.numeric(Player[7:10]))/as.numeric(Player["FGA"])) {
    Shooting = c(16,17,18,19,20,21,22)
    Func = (1/360)*(Shooting-22)^2+0.1
    rand = runif(1)
    while (rand > sum(Func[1:7])) {
      rand = runif(1)
    }
    i = 1
    while (rand > sum(Func[1:i])) {
      i = i + 1
    }
    
    ShotDistance = (i - 1) + 16
    TrueShot = ShotDistance + runif(1)
    
    Toprange = (as.numeric(Player["10to16c"]) + as.numeric(Player["16to3c"]))/2
    Lowrange = (as.numeric(Player["16to3c"]) + as.numeric(Player["3c"]))/2
    Aval = 49/(Toprange - Lowrange)
    ShotPercent = (1/Aval)*(TrueShot-23)^2+Lowrange
    
    if (runif(1) < ShotPercent) {
      cat(Player[1], "makes a", ShotDistance, "foot field goal.")
      nameWhiteSpace(Player)
      cat("             Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      return(c(TRUE,2,0))
    } else {
      cat(Player[1], "misses a", ShotDistance, "foot field goal.")
      nameWhiteSpace(Player)
      cat("            Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      return(c(FALSE,0,0))
    }
  }
  else if (x < sum(as.numeric(Player[7:11]))/as.numeric(Player["FGA"])) {
    S = rexp(1,2.5)
    while (S >= 1.5) {
      S = rexp(1,2.5)
    }
    ShotDistance = floor(6*S)+23
    TrueShot = 6*S+23
    
    Toprange = (as.numeric(Player["16to3c"]) + as.numeric(Player["3c"]))/2
    Lowrange = as.numeric(Player["3c"])-abs(as.numeric(Player["3c"])-as.numeric(Player["16to3c"]))
    Aval = 25/(Toprange - Lowrange)
    #cat(TrueShot,Toprange,Aval)
    ShotPercent = (1/Aval)*-(TrueShot-23)^2+Toprange
    
    if (runif(1) < ShotPercent) {
      cat(Player[1], "makes a", ShotDistance, "foot three point field goal.")
      nameWhiteSpace(Player)
      cat(" Likelihood =", format(round(ShotPercent, 4), nsmall=4))
      return(c(TRUE,3,3))
    } else {
      cat(Player[1], "misses a", ShotDistance, "foot three point field goal.")
      nameWhiteSpace(Player)
      cat("Likelihood =", format(round(ShotPercent, 4), nsmall=4),"\n")
      return(c(FALSE,0,3))
    }
  }
  
}

rebounding <- function(a,b) {
  d = 1
  x = runif(1)
  reboundSum = sum(as.numeric(b[3,1:5])) + sum(as.numeric(a[2,1:5]))
  for (i in 1:5) {
    if (x < sum(as.numeric(b[3,1:i]))/reboundSum) {
      cat(b[1,i], "defensive rebound.                     ")
      nameWhiteSpace(b[1:19,i])
      cat(" Likelihood =",format(round(as.numeric(b[3,i])/reboundSum, 4), nsmall=4), "\n")
      d = 0
      return(c(i,"dreb"))
      break
    }
  }
  if (d != 0) {
    defSum = sum(as.numeric(b[3,1:5]))/reboundSum
    for (j in 1:5) {
      if (x < defSum + sum(as.numeric(a[2,1:j]))/reboundSum) {
        cat(a[1,j], "offensive rebound.                     ")
        nameWhiteSpace(a[1:19,j])
        cat(" Likelihood =",format(round(as.numeric(b[2,i])/reboundSum, 4), nsmall=4), "\n")
        return(c(j,"oreb"))
        break
      }
    }
  }
  cat(i,j)
}

# ShotType is either a 2 or a 3
assisting <- function(Team,Player,ShotType) {
  y = 0
  z = 0
  if (ShotType == 2) {
    if (runif(1) < as.numeric(Player[21])) {
      x = runif(1)
      assistSum = sum(as.numeric(Team[20,1:5])) - sum(as.numeric(Player[20]))
      for (i in 1:5) {
        if (Player[1] == Team[1,i]) {
          y = as.numeric(Player[20])
        }
        else if (x < ((sum(as.numeric(Team[20,1:i]))-y)/assistSum)) {
          cat("      ",Team[1,i], "assists.","\n")
          z = 1
          break
        }
      }
    }
  }
  else {
    if (runif(1) < as.numeric(Player[22])) {
      x = runif(1)
      assistSum = sum(as.numeric(Team[20,1:5])) - sum(as.numeric(Player[20]))
      for (i in 1:5) {
        if (Player[1] == Team[1,i]) {
          y = as.numeric(Player[20])
        }
        else if (x < ((sum(as.numeric(Team[20,1:i]))-y)/assistSum)) {
          cat("      ",Team[1,i], "assists.","\n")
          z = 1
          break
        }
      }
    }
  }
  if (z == 1) {
    return(i)
  } else {
    return(0)
  }
}