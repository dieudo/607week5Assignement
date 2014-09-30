607week5Assignement
===================
#(1) a- is there a relation between age and preference
# (voting yes or no on cullen over Patran)
# b-is there a relationship between location where voter lives and his preference?
# (voting yes or no on cullen over Patran)

# what is the percent of yes votes reagarless of city and age?


Glasgow<-list("16-24"=c(99400,43000),"25+"=c(150400,207000))
Edinburgh<-list("16-24"=c(80100,35900),"25+"=c(143000,214800))
CullenShink_PartanBree<-data.frame("Prefence"=c("yes","no"),"Edinburgh"=Edinburgh,"Glasgow"=Glasgow)
CullenShink_PartanBree 
CullenShink_PartanBree
View(CullenShink_PartanBree)
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(plyr)

td2<-CullenShink_PartanBree %>% 
  gather(group,Votes,- Prefence)%>%
  arrange(group)
 td2
View(td2)
#question (3),percentage of yes
total<- select(td2,Votes)%>%
sum()
total
allyes<-filter(td2,Prefence=="yes")
p<-select(allyes,Votes)%>%
sum()
p
# percentage of yes
percent_yes<-p/total
percent_yes

#(question 2 is there a relation to the location)
Glas_loc<-filter(td2,grepl("Glasgow*",group))
Glas_loc
Edin_loc<-filter(td2,grepl("Edinburgh*",group))
Edin_loc
Glas_total=select(Glas_loc,Votes)%>%
sum()
Glas_yes<-filter(Glas_loc,Prefence=="yes")
G<-select(Glas_yes,Votes)%>%
sum()
Glas_per_Yes=G/Glas_total
# for Glasgow I found 0.499799 (49.99%) which mean :
#there is no leaning to yes or no based on Glasgow
Edin_total=select(Edin_loc,Votes)%>%
  sum()
Edin_yes<- filter(Edin_loc,Prefence=="yes")
E<-select(Edin_yes,Votes)%>%
  sum()
Edin_per_Yes=E/Edin_total
# for Edinburgh the percentage of yes is 47 % (0.4708737)
#we cannot say the yes is based on the location

#are votes based on age ?
Sixteen_TwentyFour<-filter(td2,grepl("*.16.24",group))
View(Sixteen_TwentyFour)
Sixten_TwentyFourTotal=select(Sixteen_TwentyFour,Votes)%>%
  sum()
Sixteen_TwentyFourYes<- filter(Sixteen_TwentyFour,Prefence=="yes")
S<-select(Sixteen_TwentyFourYes,Votes)%>%
  sum()
# i found 69%(0.6946) meaning there is a relationship between age and voting yes
#young people tend to vote yes
percent_yesOf16to24=S/Sixten_TwentyFourTotal

TwentyFive<-filter(td2,grepl("*.25.",group))
TwentyFiveTotal=select(TwentyFive,Votes)%>%
  sum()
TwentyFiveYes<- filter(TwentyFive,Prefence=="yes")
Tw <-select(TwentyFiveYes,Votes)%>%
  sum()
# i found 41 percent(0.4102) of votes yes for 25 and upper age 
# upper age tend to vote No 

percent_yes25=Tw/TwentyFiveTotal


#(5),yes after going to this process I can see that the 
#structure of the data is so important in analysis with such 
#a small messy data set,i have a write lengthy code
#i will ask which is the best structure for my data to begin with?
#I would that I would rearrange the structure of my data ,
#by putting each age group in separate column,
#by adding a new column representing the city(G or E)
#in total I will have one column of preference ,2 columns of age ,one column of city





