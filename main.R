message("---FUNDAMENTALS OF INFORMATION VISUALISATION R CODE (20311843)---")

message("---SETTING DIRECTORY AND IMPORTING LIBRARIES---")
setwd(shortPathName(getwd()))

#Import libraries used
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)

#Import data set into a data frame and specify how to read empty values
message("---IMPORTING DATA---")
titanic <- read.csv("20311843_Samaranayake_Yasinda_dataset.csv", na.string = "")

message("---CLEANING DATA---")
#Let us take a look at the summary of the data set
summary(titanic)

message("---HANDLING MISSING VALUES---")
#Checking for missing values in the data set
colSums(is.na(titanic))
#Age - 177 
#Cabin - 687
#Embarked - 2
# Since age has a decent %, we can use the mean of the column to fill the missing values for this
meanAge <- floor(mean(titanic$Age, na.rm=TRUE))
titanic$Age[is.na(titanic$Age)] <- meanAge
#Cabin has too many missing values, high % so we can simply delete that column
titanic <- subset(titanic, select = -Cabin)
#Since there are only 2 missing values in Embarked, we will replace it with S since it has the highest count by far
titanic$Embarked[is.na(titanic$Embarked)] <- 'S'
# All missing values are handled now
colSums(is.na(titanic))

message("---FURTHER CLEANING DATA---")
#CHANGE COLUMNS TO FACTORS/INTEGERS (OR MEANINGFUL TEXT)
#Convert Survived to Factor
titanic$Survived <- as.factor(titanic$Survived)
#Convert Age to Int
titanic$Age <- as.integer(titanic$Age)
#Convert Pclass/Survived to Meaningful Text
titanic <- titanic %>%
  mutate(Pclass = recode(Pclass, '1' = 'First Class', '2' = 'Second Class', '3' = 'Third Class'))
titanic <- titanic %>%
  mutate(Survived = recode(Survived, '1' = 'Survived', '0' = 'Did Not Survive'))
# We can see that the the Gender column is a numerical feature. Let's figure out if 1 is Male or Female and create a new column. 
unique(titanic$NameTitle)

message("---MERGING/MAPPING DATA---")
titanic <- transform(titanic, Gender= ifelse(NameTitle==" Mr" | NameTitle==" Dr" | NameTitle==" Master" | NameTitle==" Don" | NameTitle==" Rev" | NameTitle==" Dr" | NameTitle==" Major" | NameTitle==" Sir" | NameTitle==" Col" | NameTitle==" Jonkheer", "Male", "Female"))
allSurvived <- filter(titanic, Survived == "Survived")
allNonSurvived <- filter(titanic, Survived == "Did Not Survive")

message("---REMOVING UNNECESSARY COLUMNS---")
titanic <- subset(titanic, select = -c(PassengerId, Ticket, NameTitle))

message("---INITIAL QUESTIONS---")
message("---QUESTION 1 : How did wealth (passenger class) affect their survival?---")

allSurvived <- filter(titanic, Survived == "Survived")

pClassSurvivalPlot1 <- ggplot(allSurvived, aes(x= Pclass)) + 
  geom_bar(stat="count", fill="lightblue") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1)
pClassSurvivalPlot1 <- pClassSurvivalPlot1 + labs(title = "Survived Passengers vs Passenger Class", 
                                                  x = "Passenger Class", y="Count of Passengers that Survived") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(pClassSurvivalPlot1) #Interactive Plot
#We can see that most that survived were from First Class. To further confirm this statement, 
#We will have to look at the of % of survival since the counts are not constant.

#First class % of survival.
firstClassPassengers <- filter(titanic, Pclass == "First Class")
firstClassPassengers <- firstClassPassengers %>% 
  group_by(Survived) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(Survived))
firstClassPassengers$label <- scales::percent(firstClassPassengers$per)
firstClassPassengersPlot <- plot_ly(firstClassPassengers, labels = ~Survived, values = ~n, type = 'pie',
               textposition = 'inside', textinfo='label+percent', showlegend = TRUE, marker = list(colors = c("#C1E0F7", "#CFBAE1")), insidetextfont = list(color = 'black'))
firstClassPassengersPlot <- firstClassPassengersPlot %>% layout(title = 'Survival Rate of First Class Passengers (%)')
ggplotly(firstClassPassengersPlot)

# Second class % survival
secondClassPassengers <- filter(titanic, Pclass == "Second Class")
secondClassPassengers <- secondClassPassengers %>% 
  group_by(Survived) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(Survived))
secondClassPassengers$label <- scales::percent(secondClassPassengers$per)
secondClassPassengersPlot <- plot_ly(secondClassPassengers, labels = ~Survived, values = ~n, type = 'pie',
                                    textposition = 'inside', textinfo='label+percent', showlegend = TRUE, marker = list(colors = c("#C1E0F7", "#CFBAE1")), insidetextfont = list(color = 'black'))
secondClassPassengersPlot <- secondClassPassengersPlot %>% layout(title = 'Survival Rate of Second Class Passengers (%)')
ggplotly(secondClassPassengersPlot)

# Third class % survival 
thirdClassPassengers <- filter(titanic, Pclass == "Third Class")
thirdClassPassengers <- thirdClassPassengers %>% 
  group_by(Survived) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(Survived))
thirdClassPassengers$label <- scales::percent(thirdClassPassengers$per)
thirdClassPassengersPlot <- plot_ly(thirdClassPassengers, labels = ~Survived, values = ~n, type = 'pie',
                                     textposition = 'inside', textinfo='label+percent', showlegend = TRUE, marker = list(colors = c("#C1E0F7", "#CFBAE1")), insidetextfont = list(color = 'black'))
thirdClassPassengersPlot <- thirdClassPassengersPlot %>% layout(title = 'Survival Rate of Third Class Passengers (%)')
ggplotly(thirdClassPassengersPlot)

message("---QUESTION 2 : How did the gender affect the rates of survival?---")

#Proceeding to Question with new column named Gender
noOfMaleSurvived <- sum(allSurvived$Gender == "Male")
noOfFemaleSurvived <-sum(allSurvived$Gender == "Female")
totalSurvived <- noOfFemaleSurvived + noOfMaleSurvived

# Create dataframe with data for gender survival classification 
genderSurvivalDf <- data.frame(
  Gender = c("Male", "Female"),
  value = c(noOfMaleSurvived, noOfFemaleSurvived)
)

genderDonutPlot <- genderSurvivalDf %>% plot_ly(labels = ~Gender, values = ~value, text = ~value)
genderDonutPlot <- genderDonutPlot %>% add_pie(hole = 0.6)
genderDonutPlot <- genderDonutPlot %>% layout(title = "Survival Rate (%) of Passengers Based on Gender")
ggplotly(genderDonutPlot)

message("---QUESTION 3 : How did the age of passengers play a role in their survival?---")

#Lets separate the children from the adults for a much clearer analysis
allChildren <- filter(titanic, Age < 18)
allAdults <- filter(titanic, Age >= 18)

#From these children, we can see the % of the kids that survived
allChildrenSurvived <- filter(allChildren, Survived == "Survived")
survivedChildrenCount <- length(allChildrenSurvived$Name)
allChildrenDidntSurvive <- filter(allChildren, Survived == "Did Not Survive")
childrenDidntSurviveCount <- length(allChildrenDidntSurvive$Name)
allChildrenCount <- length(allChildren$Name)

#Make dataframe for pie chart and plot
childSurvivalDf <- data.frame(
  group = c("Survived", "Did not Survive"),
  value = c(survivedChildrenCount, childrenDidntSurviveCount),
  percentage = c(survivedChildrenCount/allChildrenCount, childrenDidntSurviveCount/allChildrenCount)
)
childSurvivalDf <- childSurvivalDf %>%
  mutate(labels = scales::percent(percentage, accuracy = 0.01))

#Construct the pie chart
childSurvivalPlotPie <- plot_ly(childSurvivalDf, labels = ~group, values = ~value, type = 'pie',
                                    textposition = 'inside', textinfo='label+percent', showlegend = TRUE, marker = list(colors = c("#C1E0F7", "#CFBAE1")), insidetextfont = list(color = 'black'))
childSurvivalPlotPie <- childSurvivalPlotPie %>% layout(title = 'Survival Rate of Children (%)')
ggplotly(childSurvivalPlotPie)

#We can also take a look at which ages in the children survived with a simple histogram
ageChildSurvivedHistPlot <- ggplot(allChildrenSurvived, aes(x=Age)) + geom_histogram(fill="lightblue", col=I("black"), bins=15, linewidth=0.1) + geom_line(aes(y = after_stat(count)), stat="density", linewidth = 0.2, colour="red", linetype=1)
ageChildSurvivedHistPlot <- ageChildSurvivedHistPlot + labs(title = "Age Distribution of Children that Survived", x = "Age", y = "Frequency of Age Bin") + theme(plot.title=element_text(hjust=0.5))
ggplotly(ageChildSurvivedHistPlot) #Interactive Plot

#Lets see the % of the adults that survived
allAdultsDidntSurvive <- filter(allAdults, Survived == "Did Not Survive")
allAdultsSurvived <- filter(allAdults, Survived == "Survived")
allAdultsCount <- length(allAdults$Name)
survivedAdultsCount <- length(allAdultsSurvived$Name)
adultDidntSurviveCount <- length(allAdultsDidntSurvive$Name)

#Make dataframe and plot graph
adultSurvivalDf <- data.frame(
  group = c("Survived", "Did not Survive"),
  value = c(survivedAdultsCount, adultDidntSurviveCount),
  percentage = c(survivedAdultsCount/allAdultsCount, adultDidntSurviveCount/allAdultsCount)
)
adultSurvivalDf <- adultSurvivalDf %>%
  mutate(labels = scales::percent(percentage, accuracy = 0.01))

adultSurvivalPlotPie <- plot_ly(adultSurvivalDf, labels = ~group, values = ~value, type = 'pie',
                                textposition = 'inside', textinfo='label+percent', showlegend = TRUE, marker = list(colors = c("#C1E0F7", "#CFBAE1")), insidetextfont = list(color = 'black'))
adultSurvivalPlotPie <- adultSurvivalPlotPie %>% layout(title = 'Survival Rate of Adults (%)')
ggplotly(adultSurvivalPlotPie)

#Lets see the ages of the adults that survived
allAdultsSurvived <- filter(allAdults, Survived == "Survived")
ageAdultSurvivedHistPlot <- ggplot(allAdultsSurvived, aes(x=Age)) + geom_histogram(fill="lightblue", col=I("black"), bins=9, linewidth=0.1) + geom_line(aes(y = after_stat(count)), stat="density", linewidth = 0.2, colour="red", linetype=1)
ageAdultSurvivedHistPlot <- ageAdultSurvivedHistPlot + labs(title = "Age Distribution of all Adults", x = "Age", y = "Frequency of Age Bin") + theme(plot.title=element_text(hjust=0.5))
ggplotly(ageAdultSurvivedHistPlot) #Interactive Plot

message("---FURTHER QUESTIONS---")
message("---QUESTION 4 : Did a man in first class have a better chance of survival vs woman in third class?---")

GenderPclassHeatMap <- ggplot(allSurvived, aes(Pclass, Gender))
GenderPclassHeatMap <- GenderPclassHeatMap + geom_bin2d() + stat_bin2d(geom = "text", aes(label = after_stat(count)))
GenderPclassHeatMap <- GenderPclassHeatMap + scale_fill_gradient(name = "Count of\nSurvived", low = "white", high = "red")
GenderPclassHeatMap <- GenderPclassHeatMap + labs(title = "Gender and Class Heatmap with Survival Count") + theme(plot.title=element_text(hjust=0.5))
ggplotly(GenderPclassHeatMap) #Interactive Plot
#(46/122) * 100 = 37.7% rate of survival for men
#(72/144) * 100 = 50% rate of survival for women

message("---QUESTION 5 : What is the effect of siblings/spouses on board?---")

# First lets take a look at the different number of siblings/spouses different people in the ship had with them
sibSpSurvivalPlot <- ggplot(titanic, aes(SibSp)) 
sibSpSurvivalPlot <- sibSpSurvivalPlot + geom_bar(fill='lightblue') + labs(title = "Sibling/Spouse Count", x = "Number of siblings/spouse", y="Count of Passengers") + theme(plot.title=element_text(hjust=0.5))
ggplotly(sibSpSurvivalPlot) #Interactive Plot
# Generally, majority of the passengers had no passengers/spouse on board with them, but we can see that were were some with 1-4, a few with 5 and some a bit with even 8
# We can see which of these people survived with a fill on Survived
sibSpSurvivalFillPlot <- ggplot(titanic, aes(x = SibSp, fill = factor(Survived))) +
  geom_bar() + labs(title = "Sibling/Spouse Count", x = "Number of siblings/spouse", y="Count of Passengers", fill = "Legend") + theme(plot.title=element_text(hjust=0.5))
ggplotly(sibSpSurvivalFillPlot) #Interactive Plot
# As we can see, people with >=5 did not survive, this maybe because they tried to save the family before them.
# Did having a specific number of spouse/siblings increase your survival rate?
# Since 0,1,2 had the most numbers, we can plot them individually

# No siblings -
noSib <- filter(titanic, SibSp == 0)
nosibSpSurvivalPlot <- ggplot(noSib, aes(Survived))
nosibSpSurvivalPlot <- nosibSpSurvivalPlot + geom_bar(fill="#788AA3") + geom_text(aes(label = after_stat(count)), stat = "count", vjust=-1) + labs(title = "Count of Passengers with\n no siblings/spouse", y="Passenger Count") + theme(plot.title=element_text(hjust=0.5))
ggplotly(nosibSpSurvivalPlot) #Interactive Plot
#210/(398+210) * 100 = 34.5% chance of survival

# 1 sibling - 
oneSib <- filter(titanic, SibSp == 1)
onesibSpSurvivalPlot <- ggplot(oneSib, aes(Survived))
onesibSpSurvivalPlot <- onesibSpSurvivalPlot + geom_bar(fill="#92B6B1") + geom_text(aes(label = after_stat(count)), stat = "count", vjust=-1) + labs(title = "Count of Passengers with\n 1 sibling/spouse", y="Passenger Count") + theme(plot.title=element_text(hjust=0.5))
ggplotly(onesibSpSurvivalPlot) #Interactive Plot
# 112/(97+112) * 100 = 53.5% chance of survival

# 2 siblings - 
twoSib <- filter(titanic, SibSp == 2)
twosibSpSurvivalPlot <- ggplot(twoSib, aes(Survived))
twosibSpSurvivalPlot <- twosibSpSurvivalPlot + geom_bar(fill="#E8DDB5") + geom_text(aes(label = after_stat(count)), stat = "count", vjust=-1) + labs(title = "Count of Passengers with\n 2 siblings/spouse", y="Passenger Count") + theme(plot.title=element_text(hjust=0.5))
ggplotly(twosibSpSurvivalPlot) #Interactive Plot
# 13/(15+13) * 100 = 46.4% chance of survival
# We can see that having just 1 sibling on board let to the best survival rate, this is because they probably help each oter and they dont have to worry about many other members.

message("---QUESTION 6 : What is the effect of having parents/children on board?---")

#The Parch variable takes into consideration of how many parents/children were on board with a passenger
#Let's see how the ranges of such cases
parchSurvivalPlot <- ggplot(titanic, aes(Parch))
parchSurvivalPlot <- parchSurvivalPlot + geom_bar(fill="lightblue") + geom_text(aes(label = after_stat(count)), stat = "count", vjust=-1) + labs(title = "Count of Parents/Children on board", x = "No. of Parents/Children On board", y="Passenger Count") + theme(plot.title=element_text(hjust=0.5))
ggplotly(parchSurvivalPlot) #Interactive Plot

# There were most with no parents/children on board but there were also people with 1-6 children/parents on board.
# We can see which of these people survived with a fill on Survived
parchSurvivalPlot1 <- ggplot(titanic, aes(x = Parch, fill = factor(Survived))) +
  geom_bar() + labs(title = "Count of Parents/Children on board", x = "No. of Parents/Children On board", y="Passenger Count", fill="Survived") + theme(plot.title=element_text(hjust=0.5))
ggplotly(parchSurvivalPlot1) #Interactive Plot

# Seems like having no, 1, 2 parents/children in board led to  a decent rate of survival. 
# Lets take a look at the different survival rates, we can ignore the number 3-6 since it has too little data for an accurate comparison
zeroOneTwoParch <- filter(titanic, Parch==0 | Parch==1 | Parch==2)
Parch <- ggplot(zeroOneTwoParch, aes(Survived, fill=Survived))
Parch <- Parch + geom_bar() + geom_text(aes(label = after_stat(count)), stat = "count", vjust=-1)
Parch <- Parch + labs(title = "Parent/Children Count vs Survival Count", x = "No. of Parents/Children", y="Count of Passengers") + theme(plot.title=element_text(hjust=0.5)) + facet_grid(. ~ Parch, margins=TRUE)
ggplotly(Parch)
#Seems like having just 1 child or parent led to the greatest survival rate, this may because they help each other.

message("---QUESTION 7 : Where did most travelers board from?---")

# The embarked column shows 3 distinct types which are S, C, Q. Each of them mean Southampton, Cherbourg and Queens town respectively.
southampton <- filter(titanic, Embarked == 'S')
cherbourg <- filter(titanic, Embarked == 'C')
queenstown <- filter(titanic, Embarked == 'Q')
countSouthampton <-length(southampton$Name)
countCherbourg <- length(cherbourg$Name)
countQueenstown <-length(queenstown$Name)
totalCount <- length(titanic$Name)
citySurvivaldDf <- data.frame(
  group = c("Southampton", "Cherbourg", "Queens Town"),
  value = c(countSouthampton, countCherbourg, countQueenstown),
  percentage = c(countSouthampton/totalCount, countCherbourg/totalCount, countQueenstown/totalCount)
)
citySurvivaldDf <- citySurvivaldDf %>%
  mutate(labels = scales::percent(percentage, accuracy = 0.01))

citySurvivaldDfPlotPie <- plot_ly(citySurvivaldDf, labels = ~group, values = ~value, type = 'pie',
                                textposition = 'inside', textinfo='label+percent', showlegend = TRUE, marker = list(colors = c("#C1E0F7", "#CFBAE1", "#E3D87E")), insidetextfont = list(color = 'black'))
citySurvivaldDfPlotPie <- citySurvivaldDfPlotPie %>% layout(title = 'Count of Passengers based on Embarkation Point')
ggplotly(citySurvivaldDfPlotPie)

#We can also get the count of the people - barplot
citySurvivaldDfPlotBar <- ggplot(citySurvivaldDf, aes(x = group, y = value)) +
  geom_col(fill='#7EB2DD') +
  geom_text(aes(label = value), vjust = 1.5, colour = "black") + labs(title = "Count of Boarding Passengers based on City", x = "Embarkation Point", y="Count of Passengers") + theme(plot.title=element_text(hjust=0.5))
ggplotly(citySurvivaldDfPlotBar)

message("---QUESTION 8 : How much did each person pay based on fair/Pclass?---")

pClassPlotBox <- ggplot(data=titanic, mapping=aes(x=factor(Pclass), y=Fare))

pClassPlotBox <- pClassPlotBox + geom_boxplot(aes(colour = Pclass)) + ylim(0, 300)
pClassPlotBox <- pClassPlotBox + labs(title = "Fair fee paid by different Classes", x="Class", y="Fair Price") + theme(plot.title=element_text(hjust=0.5))
suppressWarnings(ggplotly(plot(pClassPlotBox))) #Interactive Plot
#1st class 58, 2nd class 14.25, 3rd class 8.05

message("---QUESTION 9 : Which Embarkation Point has the lowest rate of survival? Why?---")

southampton <- filter(titanic, Embarked == 'S')
cherbourg <- filter(titanic, Embarked == 'C')
queenstown <- filter(titanic, Embarked == 'Q')

EmbarkedSurvivalHeatMap <- ggplot(titanic, aes(Embarked, Survived))
EmbarkedSurvivalHeatMap <- EmbarkedSurvivalHeatMap + geom_bin2d() + stat_bin2d(geom = "text", aes(label = after_stat(count)))
EmbarkedSurvivalHeatMap <- EmbarkedSurvivalHeatMap + scale_fill_gradient(name = "Count of\nPassengers", low = "white", high = "#E7B10A")
EmbarkedSurvivalHeatMap <- EmbarkedSurvivalHeatMap + labs(title = "Embarkation Point vs Survival Count") + theme(plot.title=element_text(hjust=0.5))
ggplotly(EmbarkedSurvivalHeatMap) #Interactive Plot
#Seems like most in Southampton did not survive, why?

southamptonFacet <- ggplot(southampton, aes(Survived, fill=Survived))
southamptonFacet <- southamptonFacet + geom_bar() + geom_text(aes(label = after_stat(count)), stat = "count", vjust=-1)
southamptonFacet <- southamptonFacet + labs(title = "Classes in Southampton Passengers vs Survival Count", x = "Class of Passenger", y="Count of Passengers") + theme(plot.title=element_text(hjust=0.5))
southamptonFacet <- southamptonFacet + facet_grid(. ~ Pclass, margins=TRUE)
ggplotly(southamptonFacet) #Interactive Plot
#Most in Southampton was in third class and as analyzed before, most in 3rd class didnt survive.

message("---UNCOMMENT TO REMOVE PLOTS AND CLEAR ENVIRONMENT, CONSOLE")

# # #Clear environment
# rm(list = ls())
# # # Clear console
# cat("\014")  # ctrl+L
# # # Clear plots
# dev.off()

message("---END OF R CODE, THANK YOU! :)")



