#Load the dataset  using the appropriate function read.csv().
diabetes_data <- read.csv("diabetes.csv")

#Create suset of the must-be non-zero variables
subset1 <- subset(diabetes_data, select = c(Insulin, Glucose, BMI, SkinThickness, BloodPressure))
boxplot(subset1, main = "Outliers")

#Create function replace_zero
replace_zero <- function(x, replacement) {
  x[x == 0] <- replacement
  return(x)
}
#Apply replace_zero for the selected variables
subset1$BloodPressure <- replace_zero(subset1$BloodPressure, mean(subset1$BloodPressure[subset1$BloodPressure != 0], na.rm = TRUE))
subset1$Glucose <- replace_zero(subset1$Glucose, mean(subset1$Glucose[subset1$Glucose != 0], na.rm = TRUE))
subset1$SkinThickness <- replace_zero(subset1$SkinThickness, mean(subset1$SkinThickness[subset1$SkinThickness != 0], na.rm = TRUE))

subset1$BMI <- replace_zero(subset1$BMI, median(subset1$BMI[subset1$BMI != 0], na.rm = TRUE))
subset1$Insulin <- replace_zero(subset1$Insulin, median(subset1$Insulin[subset1$Insulin != 0], na.rm = TRUE))

mean(subset1$Glucose)

#Concatenation of the two dfs
df <- bind_cols(select(subset1, BloodPressure, Glucose, SkinThickness, BMI, Insulin), select(diabetes_data, Pregnancies, DiabetesPedigreeFunction, Age, Outcome))

#heatmap(df)
library(psych)
describe(df)
str(df)
sd(df$Pregnancies)
#cor(df)
library(ggplot2)
library(GGally)

ggpairs(df, title="correlogram with ggpairs()") 


plot(df , pch=20 , cex=1.5, col="#69b3a2")
diab_cor <- round(cor(df[1:8]),1)
library(ggcorrplot)
ggcorrplot(diab_cor)
library(ggplot2)

ggplot(aes(x = Age), data=df) +
  geom_histogram(binwidth=1, color='black', fill = "#F79420") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("No of people by age")

hist_plot <- ggplot(df, aes(x = Glucose)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Glucose Distribution", x = "Glucose Level", y = "Frequency")

scatter_plot <- ggplot(df, aes(x = Glucose, y = BloodPressure)) +
  geom_point() +
  labs(title = "Scatter Plot of Glucose vs. Blood Pressure", x = "Glucose Level", y = "Blood Pressure")

box_plot <- ggplot(df, aes(x = Outcome, y = BMI, fill = Outcome)) +
  geom_boxplot() +
  labs(title = "Box Plot of BMI by Outcome", x = "Outcome", y = "BMI") +
  scale_fill_manual(values = c("No Diabetes" = "lightblue", "Diabetes" = "salmon"))

print(hist_plot)
print(scatter_plot)
print(box_plot)

