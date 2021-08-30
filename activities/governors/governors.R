# data set found at https://en.wikipedia.org/wiki/List_of_current_United_States_governors_by_age
library(ggplot2)
library(dplyr)

gov <- read.csv("governors.csv")

gov2 <- select(gov, state, governor,age,miss_riv,region,sex,party)

gov2$age.at.inaug <- round(gov2$age.at.inaug,0)
transform(gov2, age = as.factor(age))

## Making latex table of data set
library(xtable)
xtable(gov2)

#contingency table sex and party
table(gov$party,gov$sex)


party_colors <- c("#2E74C0", "#CB454A") # fancy colors for democratic and republican parties

# Plotting age of governor by party
ggplot(gov, aes(x=party, y=age)) + 
  geom_boxplot(fill=party_colors,alpha=.8) +
  coord_flip()

fivenum(gov$age[gov$Party=="Democratic"])

# Plotting age of governor by party and region
ggplot(gov, aes(x = miss_riv, y = age, treatment=party))+ 
  geom_boxplot(aes(fill = party)) 


# Alternatively force red and blue for parties
p <- ggplot(gov, aes(x=miss_riv, y=age, treatment=party)) + 
  geom_boxplot(fill=rep(c(party_colors),2),alpha=.8) 
p + xlab("East or West of the Mississippi River")

ggplot(gov, aes(x=Region, y=age, treatment=party)) + 
  geom_boxplot(fill=rep(c(party_colors),7),alpha=.7)


barplot(table(gov$party,gov$region),
        col=party_colors,ylab="count",main="Governors by Party and Region",legend=TRUE,las=2)

table(gov$Party,gov$Region)
table(gov$Miss_river)

xtable(table(gov$party, gov$sex))

table(gov$Party,gov$Sex,gov$Miss_river)

#messing with map plots

gov$region <- tolower(gov$State) # converts states to all lowercase to match us_states data


us_states_gov <- merge(us_states, gov, sort = FALSE)

p0 <- ggplot(data = us_states_gov,
             mapping = aes(x = long, y = lat,
                           group = group, fill = Party))
p2 <- p0 + scale_fill_manual(values = party_colors) + labs(title = "Party of Governor, 2021", fill = NULL)

p2

install.packages("maps")
library(maps)
us_states <- map_data("state")

p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)




p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = party))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2016", fill = NULL)
p2 + theme_map() 