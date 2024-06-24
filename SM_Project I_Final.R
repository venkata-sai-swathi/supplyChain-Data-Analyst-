# Importing Libraries

library(dplyr)
library(data.table)
library(ggplot2)
library(caret)
library(randomForest)
library(rpart)
library(glmnet)
library(xgboost) # if needed

# Suppressing warnings
options(warn=-1)




# Data Manipulation and Visualization Libraries
library(dplyr)
library(ggplot2)

# Machine Learning Libraries
library(caret)
library(randomForest)
library(rpart)
library(glmnet)



# Setting options
options(warn = -1) # Suppressing warnings

# Importing the dataset
raw_df <- read.csv("DataCoSupplyChainDataset.csv", fileEncoding = "latin1")
# Getting the dimensions of the data frame
dim(raw_df)

# Viewing the last few rows of the data frame
tail(raw_df)


# Checking the structure of the data frame
str(raw_df)

# Getting a statistical summary of the dataset
summary(raw_df)

# Selecting only numeric columns
numeric_columns <- sapply(raw_df, is.numeric)

# Applying the summary function to only numeric columns
summary(raw_df[, numeric_columns])


library(dplyr)

# Summarizing only numeric columns
raw_df %>%
  select_if(is.numeric) %>%
  summary()

# Counting NA values in each column
colSums(is.na(raw_df))

# Stripping Column Names
colnames(raw_df) <- trimws(colnames(raw_df))

# Extracting duplicated rows
duplicated_rows <- raw_df[duplicated(raw_df), ]

library(ggplot2)

# Calculating product counts
product_counts <- sort(table(raw_df$`Product.Name`), decreasing = TRUE)
top_product_counts <- head(product_counts, 20)

# Creating a data frame for plotting
plot_data <- data.frame(Product = names(top_product_counts), Count = as.numeric(top_product_counts))

# Creating a horizontal bar chart
ggplot(plot_data, aes(x = reorder(Product, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + # to make the bar chart horizontal
  labs(title = "Top 20 Product Names", x = "Product Name", y = "Count") +
  theme_minimal()


library(ggplot2)

# Creating a count plot for 'Department Name'
ggplot(raw_df, aes(x = `Department.Name`)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Departments", x = "Department Name", y = "Count")



# Creating a count plot for 'Market'
ggplot(raw_df, aes(x = Market)) +
  geom_bar(fill = "green") +
  theme_minimal() +
  labs(title = "Market List", x = "Market", y = "Count")



library(ggplot2)

# Calculating the top 15 categories
category_counts <- sort(table(raw_df$`Category.Name`), decreasing = TRUE)
top_categories <- head(category_counts, 15)

# Creating a data frame for plotting
plot_data <- data.frame(Category = names(top_categories), Count = as.numeric(top_categories))

# Creating a bar plot
ggplot(plot_data, aes(x = reorder(Category, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Top 15 Categories of Goods", x = "Category", y = "Count")



# Creating a histogram for 'Days for shipping (real)'
ggplot(raw_df, aes(x = `Days.for.shipping..real.`)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Days for Shipping (Real)", 
       x = "Days for Shipping (Real)", 
       y = "Frequency")



# Creating a scatter plot
ggplot(raw_df, aes(x = `Days.for.shipping..real.`, y = Sales)) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = "Scatter Plot: Days for Shipping vs Sales per Customer",
       x = "Days for Shipping (Real)",
       y = "Sales per Customer")





# Creating a box plot
ggplot(raw_df, aes(x = `Delivery.Status`, y = Sales)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  theme_minimal() +
  labs(title = "Box Plot: Delivery Status vs Sales per Customer",
       x = "Delivery Status",
       y = "Sales per Customer")




# Creating a violin plot
ggplot(raw_df, aes(x = `Shipping.Mode`, y = Sales)) +
  geom_violin(fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Violin Chart: Shipping Mode vs Sales",
       x = "Shipping Mode",
       y = "Sales")




# Creating a scatter plot
ggplot(raw_df, aes(x = `Order.Item.Discount`, y = Sales)) +
  geom_point(color = "blue") +
  ylim(0, NA) + # Setting the lower limit of y-axis to 0
  theme_minimal() +
  labs(title = "Scatter Plot: Sales vs Order Item Discount",
       x = "Order Item Discount",
       y = "Sales")



# Creating a count plot
ggplot(raw_df, aes(x = `Customer.Segment`, fill = `Delivery.Status`)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Stacked Bar Chart: Delivery Status within Customer Segment",
       x = "Customer Segment",
       y = "Count") +
  scale_fill_brewer(palette = "Set1") # Optional: For better color distinction

install.packages("corrplot")

library(corrplot)
library(RColorBrewer)
# Selecting only numeric columns
numeric_columns <- sapply(raw_df, is.numeric)

# Calculating the correlation matrix for numeric columns only
cor_matrix <- cor(raw_df[, numeric_columns], use = "complete.obs")


# Creating the correlation plot
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.5,  # Adjust tl.cex for label size
         number.cex = 0.5,  # Adjust number.cex for correlation coefficient size
         rect.width = 0.2,  # Adjust rect.width for larger boxes
         col = brewer.pal(n = 8, name = "Blues"),
         cl.lim = c(-1, 1),
         addCoef.col = "black"  # Add correlation coefficients
)

# Removing highly correlated variables
cor_matrix <- cor(raw_df[, numeric_columns], use = "complete.obs")

# Using sapply to check for constant columns
constant_columns <- sapply(raw_df, function(x) length(unique(x)) == 1)

# Removing constant columns if any
if (any(constant_columns)) {
  raw_df <- raw_df[, !constant_columns]
}

# Recomputing the correlation matrix
numeric_columns <- sapply(raw_df, is.numeric)
cor_matrix <- cor(raw_df[, numeric_columns], use = "pairwise.complete.obs")

# Applying findCorrelation
library(caret)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.99)

df <- raw_df[, -high_cor] # Removing highly correlated columns

# Removing Undesirable columns
cols_to_remove <- c('Order.Zipcode', 'Product.Description', 'Type', 'Category.Id', 'Customer.Email', 
                    'Customer.City', 'Customer.Country', 'Customer.Fname', 'Customer.Id', 'Customer.Lname', 
                    'Customer.Password', 'Customer.State', 'Customer.Street', 'Customer.Zipcode', 
                    'Department.Id', 'Latitude', 'Longitude', 'Market', 'Order.Customer.Id', 'Order.Id', 
                    'Order.Item.Cardprod.Id', 'Order.Item.Id', 'Order.Zipcode', 'Product.Category.Id', 
                    'shipping.date..DateOrders.', 'Product.Image', 'order.date..DateOrders.', 
                    'Order.City', 'Order.State', 'Order.Country', 'Product.Card.Id', 'Sales.per.customer', 
                    'Order.Profit.Per.Order', 'Order.Item.Product.Price')

df <- df[, !(names(df) %in% cols_to_remove)]

# Removing rows with missing values
df <- na.omit(df)

# Structure of the dataframe
str(df)

# Summary of the dataframe
summary(df)

# Selecting numerical features for normalization
columns_to_scale <- c('Days.for.shipping..real.',
                       'Order.Item.Discount', 'Order.Item.Discount.Rate', 
                      'Order.Item.Profit.Ratio', 'Order.Item.Quantity', 'Sales', 'Product.Price', 
                     'Order.Item.Total')
colnames(df)
# Min-Max Normalization
df_scale <- df
df_scale[columns_to_scale] <- lapply(df_scale[columns_to_scale], function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

# Display - Normalized Data
print("\nNormalized Data:")
head(df_scale)

# Convert to factor and then to numeric for label encoding (if no specific order)
# Convert 'Delivery Status' to numeric

# Alternative approach using dplyr
library(dplyr)

#df <- df%>%mutate(`Delivery.Status` = as.numeric(as.factor(`Delivery.Status`)))


# df$`Delivery.Status` <- as.numeric(as.factor(df$`Delivery.Status`))
#df$`Order.Status` <- as.numeric(as.factor(df$`Order.Status`))
#df$`Shipping.Mode` <- as.numeric(as.factor(df$`Shipping.Mode`))

# List of columns
cols <- c('Category.Name', 'Customer.Segment', 'Department.Name', 'Order.Region', 'Product.Name',
         'Order.Status', 'Shipping.Mode')

# Looping through columns to print value counts
for (col in cols) {
  cat("\nValue counts for", col, ":\n")
  print(table(df[, col]))
}



# Loading the necessary library
if (!require(fastDummies)) install.packages("fastDummies")
library(fastDummies)

# Columns to be one-hot encoded
columns_to_encode <- c('Category.Name', 'Customer.Segment', 'Department.Name', 'Order.Region', 'Product.Name')

# Performing one-hot encoding
df_encoded <- dummy_cols(df_scale, select_columns = columns_to_encode, remove_first_dummy = TRUE)

# Print the encoded data frame
print(df_encoded)
str(df_encoded)



library(caret)

# Selecting the dependent and target variables
x <- df_encoded[, !(names(df_encoded) %in% 'Sales')]
y <- df_encoded$Sales

# Setting a seed for reproducibility
set.seed(1)

# Splitting the dataset into training and test sets
trainIndex <- createDataPartition(y, p = .75, list = FALSE)
x_train <- x[trainIndex, ]
y_train <- y[trainIndex]
x_test <- x[-trainIndex, ]
y_test <- y[-trainIndex]

# Printing the shapes of the training and test sets
cat("Shape of training set:", dim(x_train), "\n")
cat("Shape of test set:", dim(x_test), "\n")

# Fitting the linear regression model
model1_LRM <- lm(y_train ~ ., data = as.data.frame(x_train))
summary(model1_LRM)

# Extracting feature weights
feature_weights_LRM <- coef(model1_LRM)

# Printing the feature weights
print("Feature Weights:")
print(feature_weights_LRM)

# Loading necessary library
if (!require(glmnet)) install.packages("glmnet")
library(glmnet)

# Preparing the data (glmnet requires matrix format)
x_train_matrix <- as.matrix(x_train)
x_test_matrix <- as.matrix(x_test)
y_train_vector <- as.vector(y_train)

# Fitting the Ridge regression model
model2_ridge <- glmnet(x_train_matrix, y_train_vector, alpha = 0, lambda = 1.0)
summary(model2_ridge)
# Predicting on the test data
y_pred <- predict(model2_ridge, s = 1.0, newx = x_test_matrix)

# Calculating performance metrics
mae2 <- mean(abs(y_test - y_pred))
mse2 <- mean((y_test - y_pred)^2)
rmse2 <- sqrt(mse2)
r22 <- cor(y_test, y_pred)^2

# Printing the performance metrics
print(paste("Mean Absolute Error (MAE):", mae2))
print(paste("Mean Squared Error (MSE):", mse2))
print(paste("Root Mean Squared Error (RMSE):", rmse2))
print(paste("R-squared (R2):", r22))

# Loading necessary library
if (!require(rpart)) install.packages("rpart")
library(rpart)

# Implementing and fitting the Decision Tree Regressor model
model3_DTR <- rpart(y_train ~ ., data = x_train, 
                    method = "anova", 
                    control = rpart.control(maxdepth = 15, 
                                            minsplit = 5, 
                                            minbucket = 7, 
                                            xval = 10))

# Making predictions on the test data
y_pred <- predict(model3_DTR, newdata = x_test)

# Evaluating the model's performance
mae <- mean(abs(y_test - y_pred))
mse <- mean((y_test - y_pred)^2)
rmse <- sqrt(mse)
r2 <- cor(y_test, y_pred)^2

# Printing the performance metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R2):", r2, "\n")


# Loading required libraries
library(caret)
library(glmnet)
library(rpart)
library(randomForest)
library(Metrics)

# Data partitioning
set.seed(123)  # For reproducibility
index <- createDataPartition(y, p = .75, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

# Fitting a model (example with linear regression)
model_lr <- lm(Sales ~ ., data = train_data)

# Predictions
predictions <- predict(model_lr, newdata = test_data)

# Performance metrics
mse <- mean((test_data$Sales - predictions)^2)
rmse <- sqrt(mse)

print(paste("MSE:", mse))
print(paste("RMSE:", rmse))


names(train_data)

#ggplot(train_data, aes(x = reorder(Category.Name, Catagor.Name.Count()), y = Catagory.Name.Count)) +
#  geom_bar(stat = "identity") +
#  coord_flip() +
#  labs(title = "Your Title", x = "Product", y = "Count")

# Selecting only numeric columns
numeric_columns <- sapply(raw_df, is.numeric)

# Computing the correlation matrix for numeric columns only

cor_matrix <- cor(raw_df[, numeric_columns], use = "complete.obs")
print(cor_matrix)
library(dplyr)

# Selecting only numeric columns
numeric_df <- raw_df %>% select_if(is.numeric)

# Computing the correlation matrix for numeric columns only
cor_matrix <- cor(numeric_df, use = "complete.obs")


# Example of correctly referencing a column with a space in its name
print(df$`Delivery Status`)



names(raw_df)



