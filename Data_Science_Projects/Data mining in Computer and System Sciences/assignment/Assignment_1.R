
# *********************************************
# DAMI Preprocessing Exercise R file
# Complete the codes to complete the assignment
# *********************************************

# 1. Import data for analysis to R environment
# Downloaded "Adult" dataset from UCI Machine Learning Repository
# URL http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# Import dataset in adult_db
# Missing values are represented as "?" in data file, make sure that R read them as missing values (NAs)
# ------------------------------------------------------------------------------------------------------ #
# use read.table(), type ?read.table for help
adult_db <- read.table(file = "C:/Users/david/Desktop/data mining/assignment_1/adult.data",
                       header = FALSE, sep = ",", na.strings = "?", 
                       strip.white = TRUE, stringsAsFactors = FALSE)
head(adult_db)

# Assign attribute names (column names) to the data we just imported
# Attribute names are in separate file "adult.names", scroll down to the bottom of this file
# Attribute names such as ("age", "workclass", "fnlwgt",...)
# Last column of the dataset adult.db with values ">50K" and "<=50K" does not have name, 
# this is the class attribute, so we just name it as "class"

colnames(adult_db) = c("age",
                       "workclass",
                       "fnlwgt",
                       "education",
                       "education_num",
                       "marital_status",
                       "occupation",
                       "relationship",
                       "race",
                       "sex",
                       "capital_gain",
                       "capital_loss",
                       "hours_per_week",
                       "native_country",
                       "class")

# 2. Check for missing values
# Write code to plot missingness and count missing values each attribute has
# Inorder to plot missingness, you need to first install "Amelia" package
# Hint: use "apply" function along columns of "adult.db", for each column (attribute) find how many NAs are there
# --------------------------------------------------------------------------------------------------------------- #

library(Amelia)
# plot missing values in data
missmap(adult_db, y.cex = 0.5, x.cex = 0.8, rank.order = FALSE, legend = FALSE, margins = c(7,2))
# HINT: use missmap()

# count number of missing values in all attributes
sum(is.na(adult_db))


# Delete records (rows) with any missing value
adult_db_nomiss <- na.omit(adult_db)

# 3. We will take only small chunk of the data for our purpose.
# So, randomly select 1000 records from among 30 thousand records in the dataset.
# ------------------------------------------------------------------------------- #
set.seed(145)
idx = sample(1:nrow(adult_db_nomiss),1500)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL



# 3a. Examine attributes of the dataset
# Plot histogram for numeric attribute "age", with 100 (for <=50K) and 50(for >50K) breaks, 
# show main title and attribute name on the plot.
# --------------------------------------------------------------------------------------------------------

par(mar=c(5,5,2,2))
#hist(adult_db_lim$age, breaks = 100, main = "Age Distribution", 
#     xlab = "Age", ylab = "frequency")

hist(adult_db_lim$age[which(adult_db_lim$class =="<=50K")], breaks = 100, 
     xlab = "age", ylab = "frequency", col = "red", main = "Age in Heart Disease")

hist(adult_db_lim$age[which(adult_db_lim$class ==">50K")], breaks = 50, 
     xlab = "age", ylab = "frequency", col = "blue", add=T)

legend(x=65, y=35, legend = c("<=50K", ">50K"),
       col=c("red", "blue"), pch = 15, cex = 0.95)


# ******************************************************* #

# 3b. Plot barchart for categorical attribute "relationship", 
# show legend, attribute name and main title for the plot.


table(adult_db_lim$relationship)

par(mar=c(3,3,2,0), las=1)

barplot(table(adult_db_lim$relationship), col=c("black", "red", "green", "blue", "grey", "purple"),
        main = "relationship of adults",
        names.arg = c("Husband", "Not-in-family", "Other-relative", "Own-child", "Unmarried", "Wife"),
        cex.names = 0.8)

legend(x=4, y=500, legend = c("Husband", "Not-in-family", "Other-relative", "Own-child", "Unmarried", "Wife"),
       col=c("black", "red", "green", "blue", "grey", "purple"), pch = 15, cex = 0.95)

# ************************************************* #


# 3c. Plot a boxplot for attribute "Age" for groups with earning "<=50K", ">50K"
# ------------------------------------------------------------------------------

boxplot(age ~ class, data = adult_db_lim, col = "red", main = "Age of adults", names = c("<=50K", ">50K"))

# 4 Create new data set from our latest dataset with only numeric attributes
# ------------------------------------------------------------------------
adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_cat <- adult_db_lim[,c("class")]

# Standardize numeric attributes in "adult_db_numeric" dataset.
# mean = 0 and sd = 1 for all numeric attributes

adult_db_num_std <- scale(adult_db_numeric)
mean(adult_db_num_std)
sd(adult_db_num_std)

# we can check the mean and standard deviation of the standardized data
apply(adult_db_num_std, 2, mean)
apply(adult_db_num_std, 2, sd)



# 5a. Run Principal Component Analysis (PCA) on the numeric dataset from above "adult_db_num_std"
# plot the first 2 principal components
# ------------------------------------------------------------------------------------------

pr.out <- prcomp(adult_db_num_std[,c(1:6)], center = TRUE, scale = TRUE)
names(pr.out)
head(pr.out$x)
summary(pr.out)



# ******** YOUR CODE TO PLOT FOR FIRST TWO PCs ****** #
# plot(), legend()

#principal_components <- pr.out$x
#plot(principal_components[,1:2], col = (1 + heart_disease_db$class), pch = 20, main = "First two PC")

adult_db_pca <- pr.out$x
head(adult_db_num_std)

n = 1 #replicate 1 new columns
adult_db_num_std = cbind(adult_db_num_std, replicate(n,adult_db_lim$class)) #replicate from column "Rate" in the df object
head(adult_db_num_std)
colnames(adult_db_num_std)
colnames(adult_db_num_std)[colnames(adult_db_num_std)==""] <- "class"
head(adult_db_num_std)

plot(adult_db_pca[,1:2], col=c("black","red")[data.frame(adult_db_num_std)$class], pch = 20, main = "First two PC")
legend("bottomright", legend = c("<=50K", ">50K"), col=c("black", "red"), pch =15, cex = 1)

      
# 5b. Plot percentage of the variance explained by principal components
# ----------------------------------------------------------------------------
# write a code to show proportion of variance explained by each Principal Component
# Standard deviation are stored as "sdev"

# proportion of variance explained by principal components
pr.var <- (pr.out$sdev)^2
pve <- pr.var/sum(pr.var)

# plot variance explained
# two plots side by side (1 row 2 columns)
# divide plot region into two
par(mfrow = c(1,2), oma = c(1,1,2,1))
plot(pve, xlab = "PC", ylab = "Variance explained", type = "b", ylim = c(0,1))
plot(cumsum(pve), xlab = "PC", ylab = "Cumulative variance", type = "b", ylim = c(0,1))
mtext("Proportion of Variance explained by PC", outer = TRUE)
par(mfrow = c(1,1))


# 5c. write answer for this as a comment using #
# ------------------------------------------------------------------------------
# How many principal components are needed to explain 50% and 90% of variance respectively
# Answer:

# According to the chart, we need 3 principal components to explain 50% of variance and we need 6 principal components to explain 90% of variance.
# 6 because if we take just 5 principal components, we will explain 85% of variance. If we want to explain at least 90% of variance, we need also
# the 6th principal component, and we will explain 100% of variance