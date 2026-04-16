# Install and load required packages
```{r}
install.packages("tidyverse")  
library(tidyverse)
```

# Set working directory and read the CSV
```{r}
setwd("C://Users//cex//Desktop")
data <- read.csv("Social media data.csv", stringsAsFactors = FALSE)
view(data)
```

# View column names to confirm structure
```{r}
colnames(data)
```

# Clean 'Sales.Revenue..USD.' column: remove dollar signs and "billion", then convert to numeric
```{r}

data$`Sales.Revenue..USD.` <- gsub("\\$", "", data$`Sales.Revenue..USD.`)
data$`Sales.Revenue..USD.` <- gsub(" billion", "", data$`Sales.Revenue..USD.`)
data$`Sales.Revenue..USD.` <- as.numeric(data$`Sales.Revenue..USD.`)
```

# Rename column to simpler name
```{r}
colnames(data)[colnames(data) == "Sales.Revenue..USD."] <- "Sales Revenue USD"
```

# Clean 'Social Media Presence' column
```{r}
data$`Social.Media.Presence` <- tolower(trimws(data$`Social.Media.Presence`))
data$`Social.Media.Presence` <- factor(data$`Social.Media.Presence`, levels = c("low", "medium", "high"))
```

# Convert other numeric columns
```{r}
data$`Follower.Count` <- as.numeric(gsub(",", "", data$`Follower.Count`))
data$`Engagement.Rate....` <- as.numeric(gsub("%", "", data$`Engagement.Rate....`))
data$`Brand.Awareness.Score` <- as.numeric(data$`Brand.Awareness.Score`)
data$`Employee.Count` <- as.numeric(gsub(",", "", data$`Employee.Count`))
```

# Rename for easier access
```{r}
colnames(data)[colnames(data) == "Follower.Count"] <- "Follower Count"
colnames(data)[colnames(data) == "Engagement.Rate...."] <- "Engagement Rate"
colnames(data)[colnames(data) == "Brand.Awareness.Score"] <- "Brand Awareness Score"
colnames(data)[colnames(data) == "Employee.Count"] <- "Employee Count"
colnames(data)[colnames(data) == "Social.Media.Presence"] <- "Social Media Presence"
```

# Check structure
```{r}
str(data)
```

# Remove NA rows for clean analysis
```{r}
data <- na.omit(data)
```

# Data transformation (log)
```{r}
data$Brand_Followers_Growth_log <- log(data$`Follower Count` + 1)
data$Sales_Performance <- log(data$`Sales Revenue USD` + 1)
data$Employee_Count_log <- log(data$`Employee Count` + 1)
```

# Summary statistics
```{r}
summary(data)
```

# Correlation matrix
```{r}
cor_matrix <- cor(data[, c("Engagement Rate", "Brand_Followers_Growth_log", "Brand Awareness Score", "Sales_Performance", "Employee_Count_log")])
print(cor_matrix
```

# Multiple regression
```{r}
# Multiple regression
model <- lm(Sales_Performance ~ `Social Media Presence` + `Engagement Rate` + Brand_Followers_Growth_log + Industry + `Brand Awareness Score` + Employee_Count_log, data = data)
summary(model)
```

# Histograms
```{r}

hist(data$`Sales Revenue USD`, main = "Sales Performance Distribution", xlab = "Sales Revenue USD", col = "lightgrey")
hist(data$`Engagement Rate`, main = "Engagement Rate Distribution", xlab = "Engagement Rate", col = "lightgrey")
hist(data$`Follower Count`, main = "Follower Count Distribution", xlab = "Follower Count", col = "lightgrey")
barplot(table(data$`Social Media Presence`), main = "Social Media Presence", xlab = "Level", col = "lightgrey")

```

# Scatter plots with regression lines
```{r}
ggplot(data, aes(x = `Social Media Presence`, y = Sales_Performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SMM Presence vs Sales", x = "Social Media Presence", y = "Log Sales Revenue")

```


```{r}
ggplot(data, aes(x = `Engagement Rate`, y = Sales_Performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Engagement Rate vs Sales", x = "Engagement Rate", y = "Log Sales Revenue")

```

```{r}
ggplot(data, aes(x = Brand_Followers_Growth_log, y = Sales_Performance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Brand Followers vs Sales", x = "Log Follower Count", y = "Log Sales Revenue")

```


