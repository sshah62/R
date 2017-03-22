library(readr)
library(rminer)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(stringr)
library(caTools)
library(readxl)
library(DescTools)
library(ggplot2)
library(formatR)

loan <-
  read.csv("C:/Users/Sanjiv Shah/Desktop/IDA Project/loan.csv")


#Data Overview

## Dimensions
dim(loan)

## Attributes (Variables) names
colnames(loan)

## Structure
str(loan)

## Summary
summary(loan)

# Data Discrepency
## It seems data dictionary file has 78 variables and actual data file only has 74.
## Let's find out which ones.

LCDD = read_excel("C:/Users/Sanjiv Shah/Desktop/IDA Project/LCDataDictionary.xlsx")

LCDD_attributes <- as.character(na.omit(LCDD$LoanStatNew))

## Attributes in loan data file
loan_attributes <- names(loan)

## Compare the two lists
setdiff(LCDD_attributes, loan_attributes)

setdiff(loan_attributes, LCDD_attributes)


## After some investigating, the data file is missing following attributes:
## fico_range_high: The upper boundary range the borrower’s FICO at loan origination belongs to.
## fico_range_low: The lower boundary range the borrower’s FICO at loan origination belongs to.
## last_fico_range_high: The upper boundary range the borrower’s last FICO pulled belongs to.
## last_fico_range_low: The lower boundary range the borrower’s last FICO pulled belongs to.
## FICO scores are good indicators of a borrower's creditworthiness.  However, We can use correlations.
## Rest of the differences are just typos

#Data Visualizations:
# Convert loan_d to a date field
loan$issue_d <-
  as.Date(gsub("^", "01-", loan$issue_d), format = "%d-%b-%Y")

#test code
temp <- loan$emp_length
temp <- as.numeric(gsub("[^\\d]+", "", temp, perl = TRUE))
purpose <- unique(loan$purpose)
purpose
Application_Type <- (loan$application_type)
as.data.frame(table(Application_Type))
#End of test code

# Raw Data Visualizations

##1. Loan Amount Distributions
dev.off()  #Must do this before any plots to avoid error
Desc(loan$loan_amnt, main = "Loan Amount Distribution", plotit = TRUE)

##2. Interst Rate Distributions
dev.off()  #Must do this before any plots to avoid error
Desc(loan$int_rate, main = "Interest Rate Distribution", plotit = TRUE)

##3. Loan Status
dev.off()  #Must do this before any plots to avoid error
Desc(loan$loan_status, plotit = TRUE)

##4 Loans by Credit Grading
dev.off()  #Must do this before any plots to avoid error
Desc(loan$grade, plotit = TRUE)

dev.off()  #Must do this before any plots to avoid error
loan_grade <-
  loan %>% select(issue_d, loan_amnt, grade) %>% group_by(issue_d, grade) %>% summarise(Amount = sum(loan_amnt))
gg_loan_grade <- ggplot(loan_grade, aes(x = issue_d, y = Amount))
gg_loan_grade + geom_area(aes(fill = grade)) + xlab("Date of Origination")
rm(loan_grade)
rm(gg_loan_grade)


##5. Loan Values by State
dev.off()  #Must do this before any plots to avoid error
data(state.regions)
temp1 <-
  merge(state.regions,
        loan,
        by.x = "abb",
        by.y = "addr_state" ,
        all.x = TRUE)
State_values <-
  temp1 %>% group_by(region) %>% summarise(value = sum(loan_amnt, na.rm = TRUE))
state_choropleth(State_values, title = "Cumulative Loan Amounts by State", num_colors = 6)
rm(temp1)
rm(State_values)
rm(state.regions)

##5 Loans by Purpose
dev.off()  #Must do this before any plots to avoid error
Desc(loan$purpose, main = "Loan Purpose", plotit = TRUE)

##7 Loans issued by year
dev.off()  #Must do this before any plots to avoid error
loans_year <-
  loan %>% select (issue_d, id) %>% group_by(as.numeric(format(issue_d, '%Y'))) %>% summarise(n = n())
loan_amt <-
  loan %>% select (issue_d, loan_amnt) %>% group_by(as.numeric(format(issue_d, '%Y'))) %>% summarise(Amount = sum(loan_amnt))

loans_year$year <- loans_year$`as.numeric(format(issue_d, "%Y"))`
loans_year <- loans_year[, -1]
loan_amt$year <- loan_amt$`as.numeric(format(issue_d, "%Y"))`
loan_amt <- loan_amt[, -1]
temp2 <- merge(loans_year, loan_amt, by.x = "year", by.y = "year")

dev.off() #Must do this before any plots to avoid error
ggplot(data = temp2, aes(x = temp2$year)) +
  geom_bar(aes(y = n, fill = "No. of Loans"),
           width = 0.7,
           stat = "identity") +
  geom_line(aes(
    y = cumsum(n),
    group = 1,
    linetype = "Cumulative Loans"
  )) +
  labs(fill = "", linetype = "") +
  theme(axis.text.x = element_text(temp2$year, angle = 90, hjust = 0)) +
  scale_x_discrete(labels = temp2$year) + xlab("Year")

dev.off()  #Must do this before any plots to avoid error
ggplot(data = temp2, aes(x = temp2$year)) +
  geom_bar(aes(y = Amount, fill = "No. of Loans"),
           width = 0.7,
           stat = "identity") +
  geom_line(aes(
    y = cumsum(Amount),
    group = 1,
    linetype = "Cumulative Amount"
  )) +
  labs(fill = "", linetype = "") +
  theme(axis.text.x = element_text(temp2$year, angle = 90, hjust = 0)) +
  scale_x_discrete(labels = temp2$year) + xlab("Year")
rm(loans_year)
rm(loan_amt)
rm(temp2)

### Remove pre-financial crisis records and some columns
loan2009plus <-
  loan[loan$issue_d > "2008-12-01",] #Remove loans issued in 2007 and 2008
loan2009plus2 <-
  loan2009plus[, c(1:3, 7:10, 12:17, 21, 25, 28, 31, 33:34, 37, 39, 53:56, 59:74)]
rm(loan, loan2009plus)

# loan2009plus2 is now the working file.  Fix up some of the columns first
summary(loan2009plus2)
temp3 <- loan2009plus2 #make a temporary copy

temp3$emp_length <-
  as.numeric(gsub("[^\\d]+", "", temp3$emp_length, perl = TRUE)) #change emp_length a number
temp3$home_ownership[temp3$home_ownership == "MORTGAGE"] <-
  "OWN" #home_ownership status
temp3$verification_status[temp3$verification_status == "Source Verified"] <-
  "Verified" #income verification

###Loan Status: If not current, issued, or Fully paid then it is in default
temp3$loan_status[temp3$loan_status == "Charged Off"] <- "Default"
temp3$loan_status[temp3$loan_status == "Late (31-120 days"] <-
  "Default"
temp3$loan_status[temp3$loan_status == "In Grace Period"] <-
  "Default"
temp3$loan_status[temp3$loan_status == "Late (16-30 days)"] <-
  "Default"
temp3$loan_status[temp3$loan_status == "Does not meet the credit policy. Status:Charged Off"] <-
  "Default"
temp3$loan_status[temp3$loan_status == "Does not meet the credit policy. Status:Fully Paid"] <-
  "Fully Paid"
temp3$loan_status[temp3$loan_status == "Late (31-120 days)"] <-
  "Default"
temp3$loan_status[temp3$loan_status == "Issued"] <- "Current"
temp3$loan_status[temp3$loan_status == "Fully Paid"] <- "Fully_Paid"
unique(temp3$loan_status)



# Missing Data
temp3_na <- temp3 %>%  summarise_each(funs(sum(is.na(.))))

summary(temp3_na)
str(temp3_na)

## Variables 23-25, 27-37 and 39-41 have > 99% NA
## Remove these columns

temp3 <- temp3[,c(1:22,26,38)]
temp3_na <- temp3 %>%  summarise_each(funs(sum(is.na(.))))
str(temp3_na)

## Now we have only four attributes with missing data each being less than 10% of Total
## Reomve the data(rows) with largest missing value (tot_cur_bal)

temp4 <- temp3[complete.cases(temp3[,24]),]
temp4 <- temp4[complete.cases(temp4[,23]),]
temp4_na <- temp4 %>%  summarise_each(funs(sum(is.na(.))))
str(temp4_na)

## now we only have 2 variables (emp_length(42,764, ~5%) and revol_util(381, <0.1%))
## These attributes are useful.  Just Remove the instances
temp4 <- temp4[complete.cases(temp4[,8]),]
temp4 <- temp4[complete.cases(temp4[,19]),]

temp4_na <- temp4 %>%  summarise_each(funs(sum(is.na(.)))) #sum of 'NA' in each of 24 columns
str(temp4_na)

## As can be seen from str(temp4_na), we now have a dataset with no missing values.
str(temp4)

## We have 2 attributes(tot_cur_bal and total_rev_hi_lim) that are seen as char rathter than numbers.
## convert these to numeric
###temp4$tot_cur_bal <- as.numeric(temp4$tot_cur_bal)
###temp4$total_rev_hi_lim <- as.numeric(temp4$total_rev_hi_lim)

## Split this in current(active) and closed(paid-off/default)
unique(temp4$loan_status) #unique values for loan_status
loan_current <-
  temp4[temp4$loan_status == "Current",] #create a data frame of current loans

loan_closed <-
  temp4[temp4$loan_status != "Current", ] #create a data frame of closed-out (paid-off or default) loans

## save these files for future sessions so that all the above work is not necessary
write_csv(
  loan_current,
  "C:/Users/Sanjiv Shah/Desktop/IDA Project/loan_current.csv",
  na = "NA",
  append = FALSE,
  col_names = TRUE
)
write_csv(
  loan_closed,
  "C:/Users/Sanjiv Shah/Desktop/IDA Project/loan_closed.csv",
  na = "NA",
  append = FALSE,
  col_names = TRUE
)
 

