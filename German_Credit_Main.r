# German Credit: Main
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 26/05/2021
# Accessible via: https://github.com/MQCyrusKwan/Assessment-4---Group-Report

# Install package manager
if(find.package("pacman") == FALSE){
    install.packages("pacman")
}

# Load packages
pacman::p_load(dplyr, ggplot2, FactoMineR, factoextra)

# Source functions from other R scripts
source(file = "German_Credit_Features.r")
source(file = "German_Credit_Analysis.r")

# Import dataset
credit_train <- read.csv(file="data/German_Credit_TRAIN.csv", na.strings='')
credit_test <- read.csv(file="data/German_Credit_TEST.csv", na.strings='')

# ------------------------------------------------------------------------------------------|
# DESCRIPTIVE ANALYSIS:

# > Check for NA values
# > Both training and test datasets appear to be completely  
# > balanced with no missing variables
# > No further imputation required
anyNA(credit_train)
anyNA(credit_test)

# > Investigate the structure of the data
# > Structure of data says all variables are numeric
str(credit_train)

# ------------------------------------------------------------------------------------------|
# FEATURE ENGINEERING:

# > Creating list of character replacements to match data dictionary
acc_bal <- c("< 0 EU", "0 <= ... < 200 EU", ">= 200 EU", "No Account")
pay_stat <- c("None Taken", "All Paid", 
              "Existing Paid", "Delay Paid", 
              "Critical Account")
purpose <- c("Car(New)", "Car(Used)", "Furniture/Equipment", 
             "Radio/Television", "Appliances", "Repairs", 
             "Education", "Vacation", "Retraining", 
             "Business", "Other")
save_ab <- c("< 100 EU", "100 <= ... < 500 EU", 
             "500 <= ... < 1000 EU", ">= 1000 EU", 
             "Unknown")
employment <- c("Unemployed", "< 1 Year", "1 <= ... < 4 Years",
                "4 <= ... < 7 Years", ">= 7 Years")
inst_rate <- c("< 20%", "20% - 25%", "25% - 35%", "> 35%")
sex_ms <- c("Male: Divorced", "Female: Divorced",
            "Male: Single", "Male: Married/Widowed",
            "Female: Single")
guarantor <- c("None", "Co-Applicant", "Guarantor")
address <- c("< 1 Year", "1 - 4 Years", "4 - 7 Years", "> 7 Years")
val_asset <- c("Real Estate", "Savings Agreement/Life Insurance", 
               "Car or Other", "Unknown")
con_credits <- c("Bank", "Stores", "None")
housing <- c("Rent", "Own", "For Free")
num_credits <- c("1", "2 or 3", "4 or 5", "Above 6")
occupation <- c("Unemployed Non-resident", "Unskilled Resident",
                "Skilled Official", "Highly Qualified")
num_dependants <- c("< 3", ">= 3")
telephone <- c("None", "Registered")
foreign <- c("Yes", "No")
cred_worth <- c("Bad", "Good")

# > Recoding columns as character variables
categories <- list(acc_bal, pay_stat, purpose, save_ab, employment,
                   inst_rate, sex_ms, guarantor, address, val_asset,
                   con_credits, housing, num_credits, occupation,
                   num_dependants, telephone, foreign, cred_worth)

train_cat <- credit_train %>%
    select(-c(ID, Duration.of.Credit..month., Credit.Amount, 
              Age..years.))

test_cat <- credit_test %>%
    select(-c(ID, Duration.of.Credit..month., Credit.Amount, 
              Age..years.))

code_train <- uncode(credit_train, train_cat, categories)
code_test <- uncode(credit_test, test_cat, categories)

# > Splitting sex & marital status to individual attributes
# > For training set
code_train <- code_train %>%
    mutate(Sex...Marital.Status, 
           Sex=ifelse(
                grepl("Male", Sex...Marital.Status), 
                      "Male", "Female"))

code_train <- code_train %>%
    mutate(Sex...Marital.Status, 
           Marital.Status=ifelse(
                            grepl("Single", Sex...Marital.Status), 
                            "Single", "Divorced/Separated/Married"))

code_train <- code_train %>%
    select(-Sex...Marital.Status)

# > For testing set
code_test <- code_test %>%
    mutate(Sex...Marital.Status, 
           Sex=ifelse(
                grepl("Male", Sex...Marital.Status), 
                      "Male", "Female"))

code_test <- code_test %>%
    mutate(Sex...Marital.Status, 
           Marital.Status=ifelse(
                            grepl("Single", Sex...Marital.Status), 
                            "Single", "Divorced/Separated/Married"))

code_test <- code_test %>%
    select(-Sex...Marital.Status)

# > Inspecting the structure of modified training and testing sets
str(code_train)
str(code_test)

# ------------------------------------------------------------------------------------------|
# UNIVARIATE ANALYSIS:
bad_credit <- code_train %>%
    filter(Creditability=="Bad")

good_credit <- code_train %>%
    filter(Creditability=="Good")

# > Numeric attributes
# > Duration of Credit
dur_hist <- ggplot(code_train, aes(x=Duration.of.Credit..month.))+
                geom_histogram(binwidth=7)+
                geom_density(aes(y=7*..count..))+
                labs(title="Histogram of Monthly Credit Duration")

dur_histB <- ggplot(bad_credit, aes(x=Duration.of.Credit..month.))+
                geom_histogram(binwidth=7)+
                geom_density(aes(y=7*..count..))+
                labs(title="Histogram of Bad Monthly Credit Duration")

dur_histG <- ggplot(good_credit, aes(x=Duration.of.Credit..month.))+
                geom_histogram(binwidth=7)+
                geom_density(aes(y=7*..count..))+
                labs(title="Histogram of Good Monthly Credit Duration")

multiplot(dur_hist, dur_histB, dur_histG)

# > Credit Amount
amount_hist <- ggplot(code_train, aes(x=Credit.Amount))+
                    geom_histogram(binwidth=1000)+
                    geom_density(aes(y=1000*..count..))+
                    labs(title="Histogram of Credit Amount")

amount_histB <- ggplot(bad_credit, aes(x=Credit.Amount))+
                    geom_histogram(binwidth=1000)+
                    geom_density(aes(y=1000*..count..))+
                    labs(title="Histogram of Bad Credit Amount")

amount_histG <- ggplot(good_credit, aes(x=Credit.Amount))+
                    geom_histogram(binwidth=1000)+
                    geom_density(aes(y=1000*..count..))+
                    labs(title="Histogram of Good Credit Amount")

multiplot(amount_hist, amount_histB, amount_histG)

# > Age
age_hist <- ggplot(code_train, aes(x=Age..years.))+
                geom_histogram(binwidth=5)+
                geom_density(aes(y=5*..count..))+
                labs(title="Histogram of Creditor Age")

age_histB <- ggplot(bad_credit, aes(x=Age..years.))+
                geom_histogram(binwidth=5)+
                geom_density(aes(y=5*..count..))+
                labs(title="Histogram of Bad Creditor Age")

age_histG <- ggplot(good_credit, aes(x=Age..years.))+
                geom_histogram(binwidth=5)+
                geom_density(aes(y=5*..count..))+
                labs(title="Histogram of Good Creditor Age")

multiplot(age_hist, age_histB, age_histG)

# > Categorical attributes
bal_bar <- ggplot(code_train, aes(x=Account.Balance, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Creditor Account Balance")
stat_bar <- ggplot(code_train, aes(x=Payment.Status.of.Previous.Credit, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Status of Previous Credit")
pur_bar <- ggplot(code_train, aes(x=Purpose, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Purpose of Loan")
val_bar <- ggplot(code_train, aes(x=Value.Savings.Stocks, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Value of Savings/Stocks")
len_bar <- ggplot(code_train, aes(x=Length.of.current.employment, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Length of Current Employment")
ins_bar <- ggplot(code_train, aes(x=Instalment.per.cent, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Instalment rate percent")
gua_bar <- ggplot(code_train, aes(x=Guarantors, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Debtors & Guarantors")
add_bar <- ggplot(code_train, aes(x=Duration.in.Current.address, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Duration in Current Address")
ass_bar <- ggplot(code_train, aes(x=Most.valuable.available.asset, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Most Valuable Assets")
con_bar <- ggplot(code_train, aes(x=Concurrent.Credits, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Concurrent Credits")
apa_bar <- ggplot(code_train, aes(x=Type.of.apartment, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Creditor Apartment Type")
noc_bar <- ggplot(code_train, aes(x=No.of.Credits.at.this.Bank, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Credits at this Bank")
occ_bar <- ggplot(code_train, aes(x=Occupation, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Creditor Occupation")
dep_bar <- ggplot(code_train, aes(x=No.of.dependents, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of No. of Dependents")
tel_bar <- ggplot(code_train, aes(x=Telephone, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Creditor Telephone")
for_bar <- ggplot(code_train, aes(x=Foreign.Worker, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Foreign Workers")
cred_bar <- ggplot(code_train, aes(x=Creditability, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Creditability")
sex_bar <- ggplot(code_train, aes(x=Sex, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Sex")
mars_bar <- ggplot(code_train, aes(x=Marital.Status, fill=Creditability))+
                geom_bar()+
                labs(title="Barplot of Marital Status")

# ------------------------------------------------------------------------------------------|
# BIVARIATE ANALYSIS:

# ------------------------------------------------------------------------------------------|
# PRINCIPAL COMPONENTS ANALYSIS:

# > Perform PCA
famd_train <- FAMD(code_train, 
                   sup.var = c(1), 
                   axes = c(1,2))

train_eigen <- get_eigenvalue(famd_train)

# > Visualize plots
fviz_eig(famd_train)

fviz_contrib(famd_train, "var", axes = 1)
fviz_contrib(famd_train, "var", axes = 2)
fviz_contrib(famd_train, "var", axes = 3)
fviz_contrib(famd_train, "var", axes = 4)
fviz_contrib(famd_train, "var", axes = 5)

# ------------------------------------------------------------------------------------------|
# EXPORT:

write.csv(code_train, file="data/GerCred_Clean_TRAIN.csv")
write.csv(code_test, file="data/GerCred_Clean_TEST.csv")