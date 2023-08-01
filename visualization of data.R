########### DATA VISUALIZATION BY LOADING AN EXTERNAL CSV FILE ###################
titanic<-read.csv(file = "C:\\Users\\SHRAVAN KUMAR RANGI\\OneDrive\\Desktop\\tested.csv")
fix(titanic)                  
ncol(titanic)
nrow(titanic)
str(titanic)
typeof(titanic)
class(titanic)
head(titanic)
names(titanic)

##### TOTAL SUMMARY OF THE TITANIC DATA ############

sapply(titanic,class)
summary(titanic)

# fix(titanic)
titanic$Survived=as.factor(titanic$Survived)
titanic$Sex=as.factor(titanic$Sex)
str(titanic)

# #Pre-processing of the data is
# important before analysis,
# so null values have to be checked and removed.

sum(is.na(titanic))
nrow(titanic)

# #drop null_train contains only 631 rows because
# (total rows in data set (808) – null value rows
#   (177) = remaining rows (631) )
dropnull_titanic=titanic[rowSums(is.na(titanic))<=0,]

#Now we will divide survived and
#dead people into a separate list from 631 rows.

survivedlist=dropnull_titanic[dropnull_titanic$Survived == 1,]

# survived-list
notsurvivedlist=dropnull_titanic[dropnull_titanic$Survived == 0,]

#Now we can visualize the number of males and females dead and survived using bar plots, histograms, and piecharts.
mytable <- table(titanic$Survived)


lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, main="Pie Chart of Survived column data\n (with sample sizes)")

#adjust plot margins
par(mar = c(1, 2, 1, 1))
# #From the above pie chart,
# we can certainly say that there is a data
# imbalance in the target/Survived column.


hist(survivedlist$Age, xlab="gender", ylab="frequency",col = "pink")

# #Now let’s draw a bar plot to visualize
# the number of males and females who were
# there on the titanic ship.

barplot(table(notsurvivedlist$Sex), xlab="gender", ylab="frequency")

# #From the bar plot above we can analyze that
# there are nearly 350 males,
# and 50 females those are not survived in titanic.

#  density plot
temp<-density(table(titanic$Fare))
plot(temp, type="n", main="Fare charged from Passengers")
polygon(temp, col="orange", border="green")

#Here we can observe that there are
# some passengers who are charged extremely high.
# So, these values can affect our analysis as they are outliers.
# Let’s confirm their presence using a box-plot.

boxplot(titanic$Fare, main="Fare charged from passengers")

