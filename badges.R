# BADGES AUTOMATIZATION
# Code for automate the creation of badges from a csv with a list of participants 
# armando.d.rivera@outlook.com

#install.packages("png")
#install.packages("gridExtra")
library(png)
library(grid)
library(gridExtra)

#read the people data from a csv. the spreadsheet should have columns for name, last name, status and year
namesFile1 = read.csv("data/names1.csv")
FirstLineNameColname = "Prénom.."
FirstLineLastNameColname = "Nom.." # if just First Name is used, put here the name of an empty column
SecondLineColname = "Je.suis.."
ThirdLineColname = "Statut.."
  
#read the background image, just modify the image, but keep the dimensions
backgroudFile<-readPNG("data/background.png") # should have the estimated size of the badge and leave white space in the down part for the name

#number of badges per page
# create a pdf in letter paper with 14 badges on each pdf. If you increase the number of badges, the size of the badges will reduce 
budgesperPage = 14
EmptyBudges = 14

#process a list for all the badges
h<-dim(backgroudFile)[1] # height background image
w<-dim(backgroudFile)[2] # width of the background image
NumberInteractions = ceiling((nrow(namesFile1)+EmptyBudges)/budgesperPage)*budgesperPage
badgesVariable <- lapply (1:NumberInteractions, function(i) {
  png("temporalbadge.png", width=w, height=h)
  par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
  plot.new()
  plot.window(0:1, 0:1)
  #fill plot with image
  usr<-par("usr")    
  rasterImage(backgroudFile, usr[1], usr[3], usr[2], usr[4])
  
  #add text
  fisrtName = if(is.na(namesFile1[i,][FirstLineNameColname])){""} else {namesFile1[i,][FirstLineNameColname]}
  lastName = if(is.na(namesFile1[i,][FirstLineLastNameColname])){""} else {namesFile1[i,][FirstLineLastNameColname]}
  secondLine = if(is.na(namesFile1[i,][SecondLineColname])){""} else {namesFile1[i,][SecondLineColname]}
  thirdLine = if(is.na(namesFile1[i,][ThirdLineColname])){""} else {namesFile1[i,][ThirdLineColname]}
  
  text(.5,.3, paste(fisrtName, lastName), cex=5) # cex size of the font 
  text(.5,.2, secondLine, cex=3)
  text(.5,.1, thirdLine, cex=3)
  
  cat ("Done with badge ", i, "of", NumberInteractions, "\n")
  #close image
  dev.off()
  rasterGrob(readPNG("temporalbadge.png", native = FALSE),
             interpolate = TRUE)
})

# create a pdf in letter paper with 14 badges on each pdf. If you increase the number of badges, the size will  reduce 
for (i in 1:floor(NumberInteractions/budgesperPage)){
  valuetest = i*budgesperPage
  pdf(paste("badgesresults",i,".pdf", sep=""), paper="letter", width = 20, height = 20)
  grid.arrange(grobs = badgesVariable[(valuetest-budgesperPage+1):valuetest], ncol =2, nrow=7)
  dev.off()
  cat ("Done with pdf ", i, "of", floor(NumberInteractions/budgesperPage), "\n")
}