#Name: Aleksei Furlong
#ID:2297345
#Assignment: Final Project

install.packages("dplyr")
install.packages("corrplot")
# remove n/a values from dataset
College2 <- na.omit(College)
# number of n/a values in original dataset
sum(is.na(College))
# total number of student applications
sum(College2$Apps)
#colSums(College2[,3])
# total number of enrollments
sum(College2$Enroll)
#colSums(College2[,5])
# average total spending without tuition
College2$costWT =  College2$Room.Board+ College2$Books+ College2$Personal
mean(College2$costWT) 
# average tuition cost for out of state students
mean(College2$Outstate)
# average tuition cost for instate students
College2$Instate= College2$Outstate * 0.43
mean(College2$Instate)
# Barplot Outstate vs Instate
barplot(c(mean(College2$Outstate),mean(College2$Instate)), col = c("blue", "green"), main = "Outstate vs Instate",xlab = "Outstate(Blue) vs Instate(Green)",ylab = "Price $")
# Private vs Public Colleges
CollegePrivate = College2 %>% select_all() %>% filter(Private == "Yes")
CollegePublic = College2 %>% select_all() %>% filter(Private == "No")
mean(CollegePrivate$Outstate)
mean(CollegePrivate$Instate)
mean(CollegePublic$Outstate)
mean(CollegePublic$Instate)
# Barplot Private vs Public
barplot(c(mean(CollegePrivate$Outstate),mean(CollegePrivate$Instate),mean(CollegePublic$Outstate),mean(CollegePublic$Instate)), col = c("blue", "red","blue","red"), main = "Private vs Public",xlab = "Outstate(Blue) Instate(Red)",ylab = "Price $",sub = "Private (left) vs Public (right)")
#Correlation Matrix
CollegeMatrix <- College2[,3:21]
CollegeMatrix <-cor(CollegeMatrix)
corrplot.mixed(CollegeMatrix,lower.col = "black",upper = "square")
col <- colorRampPalette(c("white","black"))(20)
corrplot(CollegeMatrix,method = "color",order = "hclust", col = col, bg = "white", outline = "white", t1.col = "black", t1.srt = 45)
# enrollment rate greater than 90%
College2$TotalEnrollment = College2$F.Undergrad + College2$P.Undergrad
College2$EnrollmentPercentage = College2$F.Undergrad/College2$TotalEnrollment
sum(College2$EnrollmentPercentage >= 0.9)
# acceptance rate vs fulltime enrollment rate
College2$AcceptanceRate = College2$Accept/College2$Apps
CollegeGreater = College2 %>% select_all() %>% filter(College2$EnrollmentPercentage >= 0.9)
CollegeLess = College2 %>% select_all() %>% filter(College2$EnrollmentPercentage <= 0.9)
mean(CollegeGreater$AcceptanceRate)
mean(CollegeLess$AcceptanceRate)
# acceptance rate of colleges with less than 90% full time enrollment is LOWER
# Difference in Acceptance rate betwen college and university
College2$College = grepl("College",College2$Name)
College2Collegename = College2 %>% select_all() %>% filter(College2$College == "TRUE")
College2Universityname = College2 %>% select_all() %>% filter(College2$College == "FALSE")
mean(College2Collegename$AcceptanceRate)-mean(College2Universityname$AcceptanceRate)
# difference is 2.19%
# ggplot
ggplot(data = College2,aes(x=College2$perc.alumni,y=College2$Grad.Rate))+geom_smooth()+geom_point()





