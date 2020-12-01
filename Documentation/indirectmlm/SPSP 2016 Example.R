#### Load Packages ####
library( boot )
source( "indirectMLM.R" )

#### Read Data ####
attitudes.data <- read.csv( "Multigroup Attitudes.csv" )

#### Descriptives ####
demographics.data <- attitudes.data[!duplicated(attitudes.data$ID), 1:4]
with( demographics.data, {
  print( paste("Total N:", length( ID ) ) )
  print( paste("Mean Age:", round(mean( age ), 2) ) )
  print( paste("SD Age:", round(sd( age ), 2) ) )
  print( paste("Age Range: [", range( age )[1], ", ", range( age )[2], "]", sep="" ) )
  print( paste("Sex Distribution:" ) )
  print( paste(round(table(sex)/sum( table(sex) )*100, 2), "% ", names( table(sex) ), sep="" ) )
  print( paste("Ethnic Distribution:" ) )
  print( paste(round(table(ethnicity)/sum( table(ethnicity) ), 2)*100, "% ", names( table(ethnicity) ), sep="" ) )
})

#### Analyses ####
# Note: This will probably take a while to run! Be patient!
mediated.mlm <- boot( data=attitudes.data, statistic=indirect.mlm, R=1000, strata=attitudes.data$ID, 
                      y="warmth", x="target", mediator="sympathy", group.id="ID",
                      between.m=T, uncentered.x=F )

#### Print Output ####
indirect.mlm.summary( mediated.mlm )
