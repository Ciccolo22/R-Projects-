##load data

data("TravelMode")

##inspect data
head(TravelMode)

##get counts
xtabs(~mode+choice,TravelMode)


library(mlogit)

CM <- mlogit.data(TravelMode,choice = 'choice', shape='long', 
                  chid.var = 'individual',alt.var = 'mode', drop.index = TRUE)

CM
# estimate with mlogit
ml.TM <- mlogit(choice ~ gcost +wait +travel, CM, reflevel = "car")
#show results
summary(ml.TM)


ml.TM$fitted.values

apply(fitted(ml.TM, outcome=FALSE), 2, mean) # fitted mean choice probability

