getwd()

beer <- read.csv("Beer_SaoPaulo.csv")
install.packages("ggdensity")

#-------------DATA CLEANING----------------#

#Renaming the column name for clarity and increased readability
colnames(beer) <- c('Date', 'Average Temp','Min Temp', 'Max Temp', 'Consumption',
                     'Mesoregion','Microregion', 'Municipality')

#Changing Date from Char to Date
beer$Date <- as.Date(beer$Date)
class(beer$Date)


#------------MONTH VARIABLE-------------------#

#Adding Month variable based on the dates
beer$Month <- format(as.Date(beer$Date, format="%d/%m/%Y"),"%m")


#-------------EDA---------------#

#Multiple density plots for min and max temp values of beer
par(mfrow=c(1,1))
plot(density(beer$`Min Temp`), xlim = c(10,40), col="red", 
     main = "Mix and Max Temperatures (2015)",
     xlab= "Temperature")
lines(density(beer$`Max Temp`))

summary(beer$`Min Temp`)
summary(beer$`Max Temp`)

#Beer Consumption by Location
agg <- aggregate(x=beer$Consumption,
                 by= list(beer$Mesoregion),
                 FUN = sum)
agg<- agg[order(agg$x),]

ggplot(agg,
       aes(x = x,
           xend = 0,
           y = Group.1 ,
           yend = Group.1)) +
  geom_segment() +
  geom_point(size=3)+ 
  labs(x = "Count",y = "Regions ",title = "Region-wise Beer Consumption")+
  theme(panel.border = element_rect(color = "pink", fill='NA', size = 2))




#Normality check
par(mfrow=c(2,2))
hist(beer$`Average Temp`, ylim = c(0,100), col = "lightyellow", main ="Average Temp Frequency", xlab = "Average Temp")
hist(beer$Consumption, ylim= c(0,80), col = "pink", main ="Comsumption Frequency", xlab = "Beer Consumption")
hist(beer$`Min Temp`, ylim=c(0,80), col = "lightblue", main ="Min Temperature Frequency", xlab = "Min Temp")
hist(beer$`Max Temp`, ylim=c(0,100), col = "lightgreen", main ="Max Temperature Frequency", xlab = "Max Temp")

#----------------SUBSETS-----------------#

subset1 <- beer[beer$Mesoregion == "Bauru",]
subset2 <-  beer[beer$Mesoregion == "Piracicaba",]


#---------------DUMMY VARIABLE------------#

#Adding Weekend dummy variable based on the days.
checkweekend <- weekdays(beer$Date)
beer$Weekends <- ifelse(checkweekend == "Sunday" | checkweekend == "Saturday",1,0)
beer$Weekends <- factor(beer$Weekends)

View(beer)

#--------------T TEST--------------------#


#Question: Mean of Max temp for the Max beer consumption mesoregion is equal to Mean of Max temp 
#for the Min beer consumption mesoregion
#Null : Mean is the same
#Alt : Mean is not the same

t.test(subset1$`Max Temp`, subset2$`Max Temp`, alternative = "two.sided")


#Question: 
#Null = Mean Consumption is same for weekdays and weekends
#Alt = Mean of weekends > Mean of weekdays

subset3 <- head(beer[beer$Weekends == '1', ],n=30)  #Sat and Sun
subset4 <- head(beer[beer$Weekends == '0', ],n=30)  #Weekdays

t.test(subset3$Consumption, subset4$Consumption, alt="greater")



#-------------- SEASON-WISE SUBSETS------------#

#December to Feb
summer <- beer[beer$Month == '12'| beer$Month == '01' | beer$Month == '02', ]

#June to September
winter <- beer[beer$Month == '06'| beer$Month == '07' | beer$Month == '08' | beer$Month == '09' , ]

#March to June
monsoon <- beer[beer$Month == '03'| beer$Month == '04' | beer$Month == '05', ]


#----------- CORRELATION SEASON WISE ---------------#

#Summer
summer$Weekends <- as.numeric(summer$Weekends)
sumnum <- summer[, c("Consumption","Min Temp", "Max Temp", "Weekends")]
matrix <- cor(sumnum)
ggcorrplot(matrix,  hc.order = FALSE,
           lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"))+
  labs(title="Summer Correlation")


#Winter
winter$Weekends <- as.numeric(winter$Weekends)
winnum <- winter[, c("Consumption","Min Temp", "Max Temp", "Weekends")]
matrix2 <- cor(winnum)
ggcorrplot(matrix2,  hc.order = FALSE,
           lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"))+
  labs(title="Winter Correlation")



#Monsoon
monsoon$Weekends <- as.numeric(monsoon$Weekends)
rainnum <- monsoon[, c("Consumption","Min Temp", "Max Temp", "Weekends")]
matrix3 <- cor(rainnum)
ggcorrplot(matrix3,  hc.order = FALSE,
           lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"))+
  labs(title="Monsoon Correlation")





#---------- REGRESSION ON GGPLOT WITH RESIDUALS -----------#


#Summer
test <- lm(summer$Consumption~summer$`Max Temp`)
test$predict <- predict(test)
ggplot(summer, aes(x= summer$`Max Temp`, y = summer$Consumption,)) +
  geom_point()+
  geom_smooth(method=lm)+
  geom_segment(aes(xend = summer$`Max Temp`, yend = test$predict), alpha = 0.5)+
  labs(x= "Max Temp", y= "Beer Consumption", title="Beer Consumption at Max Temp in Summer")

summary(test)

#Winter
test1 <- lm(winter$Consumption~winter$`Max Temp`)
test1$predict <- predict(test1)
ggplot(winter, aes(x= winter$`Max Temp`, y = winter$Consumption,)) +
  geom_point()+
  geom_smooth(method=lm)+
  geom_segment(aes(xend = winter$`Max Temp`, yend = test1$predict), alpha = 0.5)+
  labs(x= "Max Temp", y= "Beer Consumption", title="Beer Consumption at Max Temp in Winter")

summary(test1)

#Monsoon
test2 <- lm(monsoon$Consumption~monsoon$`Max Temp`)
test2$predict <- predict(test2)
ggplot(monsoon, aes(x= monsoon$`Max Temp`, y = monsoon$Consumption,)) +
  geom_point()+
  geom_smooth(method=lm)+
  geom_segment(aes(xend = monsoon$`Max Temp`, yend = test2$predict), alpha = 0.5)+
  labs(x= "Max Temp", y= "Beer Consumption", title="Beer Consumption at Max Temp in Monsoon")

summary(test2)

test3 <- lm(beer$Consumption~beer$`Max Temp`)
summary(test3)

#-----#






