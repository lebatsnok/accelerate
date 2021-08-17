#'Make cutoffs
#'
#'Make cutoffs
#'
#'
#'@param Name name
#'@param Epoch epoch
#'@param ... dots
#'@return cutoffs
#'@export
make.cutoffs <- function(Name, Epoch, ...) structure(list(c(Epoch=Epoch, ...)), names=Name)



#'Sirard cutoffs
#'
#'Sirard cutoffs
#'
#'
#'@export
Cutoffs.Sirard <- make.cutoffs("Sirard", 15, 
                   Sedentary = 0, Light = 398, Moderate= 890, Vigorous = 1254)


#'Pate cutoffs
#'
#'Pate cutoffs
#'
#'
#'@export
Cutoffs.Pate <- make.cutoffs("Pate", 15,
                   Light = 0, Moderate = 420, Vigorous = 842)



#'Evenson cutoffs
#'
#'Evenson cutoffs
#'
#'
#'@export
Cutoffs.Evenson <- make.cutoffs("Evenson", 60, Sedentary = 0,
                   Light = 100, Moderate = 2296, Vigorous = 4012)


#'Puyau cutoffs
#'
#'Puyau cutoffs
#'
#'
#'@export
Cutoffs.Puyau <- make.cutoffs("Puyau", 60,
                   Sedentary = 0, Light = 800, Moderate = 3200, Vigorous = 8200)


#'Reilly cutoffs
#'
#'Reilly cutoffs
#'
#'
#'@export
Cutoffs.Reilly <- make.cutoffs("Reilly", 60,
                   Inactive = 0, Active = 1100)


#'RP cutoffs
#'
#'RP cutoffs
#'
#'
#'@export
Cutoffs.RP <- make.cutoffs ("RP", 60,
                   Sedentary = 0, Light = 1100, Moderate = 3200, Vigorous = 8200)


#'ENSU cutoffs
#'
#'ENSU cutoffs
#'
#'
#'@export
Cutoffs.ensu <-  make.cutoffs("ENSU", 60,
                   Light=0, Moderate = 1952, Hard =5724, VeryHard=9498)


#'Pate2 cutoffs
#'
#'Pate2 cutoffs
#'
#'
#'@export
Cutoffs.Pate2 <- make.cutoffs("Pate", 15, Sedentary= 0, Light=38, Moderate=420, Vigorous=842)


#'Ghent cutoffs
#'
#'Ghent cutoffs
#'
#'
#'@export
Cutoffs.Ghent <- make.cutoffs("Ghent", 15, Sedentary=0, Light=373, Moderate=585, Vigorous=881)


#'Default cutoffs
#'
#'Default cutoffs
#'
#'
#'@export
Cutoffs <- c(Cutoffs.Sirard, Cutoffs.Pate)

#'T1 cutoffs
#'
#'T1 cutoffs
#'
#'
#'@export
Cutoffs.T1 <- c(Cutoffs.Sirard, Cutoffs.Pate2, Cutoffs.Puyau, Cutoffs.RP, Cutoffs.Ghent, Cutoffs.Evenson)

#'3dnx cutoffs
#'
#'3dnx cutoffs
#'
#'
#'@export
Cutoffs.3dnx <- make.cutoffs(Name = "Tdnx", Epoch = 5,
                   Light = 0,     Moderate = 106,    Vigorous = 281)


#'Freedson 1998 cutoffs
#'
#'Freedson 1998 cutoffs
#'
#'
#'@export
Cutoffs.Freedson98 <- make.cutoffs("Fr98", 60, Sedentary=0, Light=100,Lifestyle=760,Moderate=1952,Vigorous=5725,VeryVigorous=9499)


#'Troiano cutoffs
#'
#'Troiano cutoffs
#'
#'
#'@export
Cutoffs.Troiano <- make.cutoffs("Tr", 60, Sedentary = 0, Light=100, Moderate = 2020, Vigorous = 5999)

#'Freedson VM cutoffs
#'
#'Freedson VM cutoffs
#'
#'
#'@export
Cutoffs.FreedsonVM <- make.cutoffs("FRVM", 60, Sedentary=0, Light=100) ###


#'Helena cutoffs
#'
#'Helena cutoffs
#'
#'
#'@export
Cutoffs.Helena <- make.cutoffs("Helena", 60, Sedentary=0, Light=100, Moderate = 2000, Vigorous=4000)

