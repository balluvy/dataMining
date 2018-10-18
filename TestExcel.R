library(dplyr)

# Load the Census data
MyData <- read.csv(file="CensusIncomedata.csv", header=TRUE, sep=",")
summary(MyData$mig_prev_sunbelt)

#delete column because there are too much missing data
cleaned_data  <- MyData
cleaned_data$mig_prev_sunbelt <- NULL
cleaned_data$mig_chg_msa <- NULL
cleaned_data$mig_chg_reg <- NULL


cleaned_data$mig_move_reg <- NULL
cleaned_data <- distinct(cleaned_data)

summary(cleaned_data)

#eleminate some missing data in those 3 column because there are aournd 8000 objects
cleaned_data$country_father = as.character(cleaned_data$country_father)
cleaned_data$country_mother = as.character(cleaned_data$country_mother)
cleaned_data$country_self = as.character(cleaned_data$country_self)

vector_of_missing_data <- c()
count = 1
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$country_father[i] == " ?" || 
     cleaned_data$country_mother[i] == " ?" ||
     cleaned_data$country_self[i] == " ?"){
    vector_of_missing_data[count] <- i
    count = count + 1
  }
}
cleaned_data <- cleaned_data[-vector_of_missing_data, ]

cleaned_data$country_father = as.factor(cleaned_data$country_father)
cleaned_data$country_mother = as.factor(cleaned_data$country_mother)
cleaned_data$country_self = as.factor(cleaned_data$country_self)

# change NA to Do not know
cleaned_data$hisp_origin = as.character(cleaned_data$hisp_origin)
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$hisp_origin[i] == " NA"){
    cleaned_data$hisp_origin[i] <- " Do not know"
  }
}
cleaned_data$hisp_origin = as.factor(cleaned_data$hisp_origin)

# change ? to not in universe because not in universe has too much data and ? should be in this column without change the data pattern 
cleaned_data$state_prev_res = as.character(cleaned_data$state_prev_res)
for(i in 1:nrow(cleaned_data)){
  if(cleaned_data$state_prev_res[i] == " ?"){
    cleaned_data$state_prev_res[i] <- " Not in universe"
  }
}
cleaned_data$state_prev_res = as.factor(cleaned_data$state_prev_res)

summary(cleaned_data)


#1. histrogram for Age
hist(cleaned_data$age,main="Histogram for Age",xlab="Age",xlim=c(0,100))

#2. pie Chart education
slices2 <- table(cleaned_data$education)
lbls2 <- cleaned_data$education
pct2 <- round(slices2/sum(slices2)*100)
lbls2 <- paste(lbls2, pct2) # add percents to labels 
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels 
pie(slices2,labels = lbls2,main="Pie Chart of education")

#3. pie Chart household summary in household
slices3 <- table(cleaned_data$det_hh_summ)
lbls3 <- cleaned_data$det_hh_summ
pct3 <- round(slices3/sum(slices3)*100)
lbls3 <- paste(lbls3, pct3) # add percents to labels 
lbls3 <- paste(lbls3,"%",sep="") # ad % to labels 
pie(slices3,labels = lbls3,main="Pie Chart of household summary in household")


##QQ Plots use to check whether data can be assumed normally distributed
##plot the kth smallest observation against the expected value of the kth smallest observation 
##out of n in a standard normal distribution.
## expect to obtain a straight line if data come from a normal distribution 
##with any mean and standard deviation.

#4. QQ Plots for capital_gains
qqnorm(table(cleaned_data$capital_gains))

#5. QQ Plots for  Capital_losses
qqnorm(table(cleaned_data$capital_losses))

#6. pie Chart class_worker
slices6 <- table(cleaned_data$class_worker)
lbls6 <- cleaned_data$class_worker
pct6 <- round(slices6/sum(slices6)*100)
lbls6 <- paste(lbls6, pct6) # add percents to labels 
lbls6 <- paste(lbls6,"%",sep="") # ad % to labels 
pie(slices6,labels = lbls6,main="Pie Chart of class worker")

#7. pie Chart Full- or part-time employment status
slices7 <- table(cleaned_data$full_or_part_emp)
lbls7 <- cleaned_data$full_or_part_emp
pct7 <- round(slices7/sum(slices7)*100)
lbls7 <- paste(lbls7, pct7) # add percents to labels 
lbls7 <- paste(lbls7,"%",sep="") # ad % to labels 
pie(slices7,labels = lbls7,main="Pie Chart of Full- or part-time employment status")

#8. pie Chart Major_occ_code
slices8 <- table(cleaned_data$major_occ_code)
lbls8 <- cleaned_data$major_occ_code
pct8 <- round(slices8/sum(slices8)*100)
lbls8 <- paste(lbls8, pct8) # add percents to labels 
lbls8 <- paste(lbls8,"%",sep="") # ad % to labels 
pie(slices8,labels = lbls8,main="Pie Chart of Major_occ_code")

#9 histogram Weeks_worked
par(cex.axis=1.2)
hist(cleaned_data$weeks_worked,main="Histogram for weeks worked",xlab="weeks worked")

test <- cleaned_data
test <- test[order(test$major_occ_code),]

######10. major_ind_code
par(cex.axis=0.05)
#10. major_ind_code
barplot(table(cleaned_data$major_ind_code),main="Bar plot for major industry code",xlab="major industry code",col=topo.colors(24))
legend("topright", c("Agriculture","Armed Forces","Business and repair services","Communications",
                     "Construction","Education","Entertainment","Finance insurance and real estate",
                     "Forestry and fisheries","Hospital services","Manufacturing???durable goods",
                     "Manufacturing???nondurable goods","Medical except hospital","Mining","Not in universe or children",
                     "Other professional services","Personal services except private HH",
                     "Private household services","Public administration","Retail trade","Social services","Transportation",
                     "Utilities and sanitary services","Wholesale trade")
       ,fill=topo.colors(24), ncol = 4,
       cex = 0.3)

#legend("topright", inset=.05, title="Number of Cylinders", table(cleaned_data$major_ind_code), cex=0.8, fill=cols)
#legend("topright", inset=.05, title="Number of Cylinders",
#       legend =cleaned_data$major_ind_code, fill=terrain.colors(3), horiz=TRUE)

#tab0 <- table(cleaned_data$major_ind_code)
#barplot(tab0,names.arg=rownames(tab0))
#legend("topright", inset=.05, title="Number of Cylinders",arg=rownames(tab0), fill=terrain.colors(3), horiz=TRUE)
 
#plot(cleaned_data$income_50k,cleaned_data$citizenship, main="Example 1", xlab = "Age", ylab = "education")
#plot(~income_50k+citizenship+own_or_self,data=cleaned_data, main="Simple Scatterplot Matrix", col=education)


plot(cleaned_data$age, cleaned_data$wage_per_hour)
cor(cleaned_data$age, cleaned_data$wage_per_hour)

#function to Stratified Sampling
function1 <- function(checkPosNeg,table){
  roww <- nrow(table)
  coll <- ncol(table)
  countpulMinus = 0
  testPosNeg = checkPosNeg
  for( i in 1:roww){
    st <- toString(table[i,coll])
    ##table for "50000+"
    if(checkPosNeg == " 50000+."){
      if(st == " 50000+." && countpulMinus == 0){
        countpulMinus = countpulMinus+1
          write.table(table[i,], "newPos.csv", quote=FALSE, sep=",")
      } 
      else if(st == " 50000+."){
        countpulMinus = countpulMinus+1
          write.table(table[i,], "newPos.csv", quote=FALSE, sep=",",append=TRUE, col.names=FALSE)
      }
    }else if(checkPosNeg == "-50000"){
    ##table for "-50000"
      if(st == "-50000" && countpulMinus == 0){
        countpulMinus = countpulMinus+1
        write.table(table[i,], "newMinus.csv", quote=FALSE, sep=",")
      }
      else if(st == "-50000"){
        countpulMinus = countpulMinus+1
        write.table(table[i,], "newMinus.csv", quote=FALSE, sep=",",append=TRUE, col.names=FALSE)
      }
    }
  }
  
  return(countpulMinus)
}


#input for 50000+ table -> " 50000+."
#input for -50000 table -> "-50000"
ans <- function1("-50000",cleaned_data) 
print(ans)
