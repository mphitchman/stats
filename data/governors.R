# data set found at https://en.wikipedia.org/wiki/List_of_current_United_States_governors_by_age
library(ggplot2)
library(dplyr)

df <- read.csv("governors.csv")
df$age.at.inaug <- round(df$age,0)
gov <- select(df, state, governor,age.at.inaug,miss_riv,region,sex,party)



#contingency table sex and party
table(gov$party,gov$sex)


party_colors <- c("#2E74C0", "#CB454A") # fancy colors for democratic and republican parties

# Plotting age of governor by party
ggplot(gov, aes(x=party, y=age.at.inaug)) + 
  geom_boxplot(fill=party_colors,alpha=.8) +
  coord_flip()

fivenum(gov$age.at.inaug[gov$party=="Democratic"])
fivenum(gov$age.at.inaug[gov$party=="Republican"])

fivenum(gov$age.at.inaug[gov$miss_riv=="W"])
fivenum(gov$age.at.inaug[gov$miss_riv=="E"])


# Plotting age of governor by party and region
ggplot(gov, aes(x = miss_riv, y = age.at.inaug, treatment=party))+ 
  geom_boxplot(aes(fill = party)) 


# Alternatively force red and blue for parties
p <- ggplot(gov, aes(x=miss_riv, y=age.at.inaug, treatment=party)) + 
  geom_boxplot(fill=rep(c(party_colors),2),alpha=.8) 
p + xlab("East or West of the Mississippi River")

ggplot(gov, aes(x=region, y=age.at.inaug, treatment=party)) + 
  geom_boxplot(fill=rep(c(party_colors),7),alpha=.7)


barplot(table(gov$party,gov$region),
        col=party_colors,ylab="count",main="Governors by Party and Region",legend=TRUE,las=2)

table(gov$party,gov$region)
table(gov$miss_riv)






## Making latex table of data set
library(xtable)
xtable(gov)

xtable(table(gov$party, gov$sex))
