setwd("~/be623_assignment5")
library(dplyr)
library(ggplot2)

# Question 1
# Building a data frame called circ_df with four columns:

# Sample, Gene, ZT and Expression, containing the 12 measurements given in the table
circ_df <- data.frame(
  Sample = paste0("S", 1:12), # Sample contains the sample IDs from S1 to S12
  Gene = c(
    rep("PER2", 6),
    rep("BMAL1", 6)
  ), # rep() repeats value and creates 6 PER2 followed by 6 BMAL1 labels
  ZT = rep(c(0, 4, 8, 12, 16, 20), 2), # ZT contains the Zeitgeber time points from 0 to 20 hours
  Expression = c(
    1.8, 3.2, 6.5, 9.4, 7.1, 3.6,
    8.6, 5.9, 2.7, 1.5, 3.4, 6.8
  ) # Expression contains the corresponding gene expression values
)
# Displaying the structure of circ_df
# str() is used to display the structure and data type of each variable
str(circ_df)

# Question 2
# Selecting the six PER2 rows using base R indexing (select rows where Gene is PER2)
per2_rows <- circ_df[circ_df$Gene == "PER2", ]
# Displaying the PER2 rows (contains the six PER2 samples)
per2_rows
# Calculating its mean, median and standard deviation
# Calculating mean expression (the average PER2 expression)
mean(per2_rows$Expression)
# Calculating median expression (the middle PER2 expression value)
median(per2_rows$Expression)
# Calculating standard deviation of PER2 expression
sd(per2_rows$Expression)

# Question 3
# Save circ_df as a CSV file and read it back into R:

# row.names = FALSE prevents R from adding row numbers as an extra column
write.csv(circ_df, "circadian_expression.csv", row.names = FALSE)
# read.csv() reads the saved CSV file back into R
circ_reloaded <- read.csv("circadian_expression.csv")
# identical() checks whether the original and reloaded Expression columns are identical
# Check whether the Expression column survived the round trip
identical(circ_df$Expression, circ_reloaded$Expression)

# Question 4
# Arranging each gene from highest to lowest expression:

# filter() keeps only the rows for the selected gene
# arrange(desc(Expression)) sorts expression from highest to lowest
per2_peak <- circ_df %>%
  filter(Gene == "PER2") %>%
  arrange(desc(Expression))
bmal1_peak <- circ_df %>%
  filter(Gene == "BMAL1") %>%
  arrange(desc(Expression))
# Displaying the sorted data
per2_peak
bmal1_peak
# Showing the ZT of the peak expression
# First row therefore represents the peak expression
per2_peak$ZT[1]
bmal1_peak$ZT[1]
# PER2 reaches its peak at ZT12
# BMAL1 reaches its peak at ZT0

# Question 5
# Add a TRUE/FALSE column called dark:
# TRUE means the sample was taken at ZT12 or later.

# mutate() adds a new column called dark
circ_df2 <- circ_df %>%
  mutate(dark = ZT >= 12)
# Display the new data frame
circ_df2
# Summarize the dark samples
circ_df2 %>%
  filter(dark == TRUE) %>% # filter() selects dark samples
  group_by(Gene) %>% # group_by() separates the results by gene
  summarize( 
    mean_expression = mean(Expression),
    n = n()
  )
# summarize() calculates the mean expression and number of samples
# Summarize the light samples
circ_df2 %>%
  filter(dark == FALSE) %>% # filter() selects light samples
  group_by(Gene) %>%
  summarize(
    mean_expression = mean(Expression),
    n = n()
  )

# Question 6
# Creating a scatter plot showing expression across ZT for each gene:

rhythm_plot <- ggplot(circ_df,aes(x = ZT, y = Expression, color = Gene)) +
  geom_point(size = 3) + # geom_point() creates the scatter plot
  # scale_color_manual() sets the two colours
  scale_color_manual(values = c("PER2" = "blue", "BMAL1" = "red")) +
  # labs() adds the title and axis labels
  labs(title = "Gene Expression Across Zeitgeber Time",x = "Zeitgeber time (h)",y = "Expression") +
  theme_minimal() # theme_minimal() gives the plot a clean appearance
# Displaying the plot
rhythm_plot
# Saving the plot
ggsave(
  "rhythm_plot.png",
  plot = rhythm_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# Question 7
# Creating a box plot comparing expression between the two genes:

expression_boxplot <- ggplot(circ_df,aes(x = Gene, y = Expression, fill = Gene)) +
  # fill = Gene gives each gene its own colour
  geom_boxplot() + # geom_boxplot() creates a box plot for each gene
  scale_fill_manual(values = c("PER2" = "blue", "BMAL1" = "red")) +
  labs(
    title = "Expression by Gene",
    x = "Gene",
    y = "Expression"
  ) +
  theme_minimal()
# Display the box plot
expression_boxplot
# Save the box plot
ggsave(
  "expression_boxplot.png",
  plot = expression_boxplot,
  width = 6,
  height = 4,
  dpi = 300
)
