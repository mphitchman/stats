# 1. Import the date file either from the web, or from a local copy in your MATH 140 folder

##stent365<-read.csv("https://www.openintro.org/data/csv/stent365.csv")
stent365<-read.csv("slides/1 Intro to Data/stent365.csv")

#2. View it!
View(stent365)


# 3. Summarize the data with table() method
(t=table(stent365)) # parenthese around everything means 'create and print'


#4 Can also summarize with proportion table.
(p <- prop.table(t,1)) # the 1 here means we're finding row proportions. 

############
## DOING THE SIMULATION
############

# 1. Create a "deck" with 378 cards marked "n", and 73 marked "s" (for a total of 451 cards)
deck<-rep(c("n","s"),times=c(378,73))

deck
# 2. The sample() method shuffles the deck, and we can specify how many cards to draw. 
# We want to draw 224 for the treatment group.

sample(deck)[1:224]

# 3. We don't really want to view the cards we've drawn, 
# just know how many 's' cards we drew.  Which we can see as follows:

table(sample(deck)[1:224]) #try running this several times!

#We can also just add up the number of 's' cards in the sample directly:

sum(sample(deck)[1:224]=="s")

# 4. Run a bunch of simulations at once, recording each time how many "s" cards in the draw

trials=10000
results=c()
for(x in 1:trials){
  results<-c(results,sum(sample(deck)[1:224]=="s"))}

results

# 5. Vizualizing these results with a histogram

hist(results,
     main=paste("Simulation Results.",trials,"trials"),
     xlab="# stroke events",
     xlim=c(20,50),
     col="light blue",
     border="brown")

# 6 How many trials gave us 45 or more "s" cards?

sum(results>=45)

# Proportion of trials with at least 45 "s" cards:
sum(results>=45)/trials







# EXTRA STUFF

#Create a function that repeatedly (n times) simulates randomly selecting 
# 224 cards to treatment group and counting the number of 's' cards
simulate <- function(n,deck){
  trial_results<-c()           
  L<-c()
  for(x in 1:n) {
    L<-c(L,table(sample(deck)[1:224])['s'])
  }
  print(paste("Number of the",n,"trials having at least 45 stroke events:",table(L>=45)[2]))
  print(paste("In other words, the proportion of times we would randomly see such extreme results is",prop.table(table(L>=45))[2]))
  return(L)
}

# Plotting the results of the simulation
sim_hist <- function(n,deck){
  pic<- hist(simulate(n,deck),
             main=paste0(n," trials of randomly assigning patients to groups"),
             xlab="# stroke events",
             xlim=c(20,50),
             col="gray",
             border="brown"
             )
  return(pic)
}


sim_histb <- function(n,L){
  pic<- hist(L,
             main=paste0(n," trials of randomly assigning patients to groups"),
             xlab="# stroke events",
             xlim=c(20,50),
             col="gray",
             border="brown"
  )
  return(pic)
}


# Running the simulation and histogram at once
trials=5000
sim <- simulate(trials,deck)
sim_histb(trials,sim)


