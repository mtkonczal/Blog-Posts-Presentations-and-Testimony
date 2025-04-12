# Load necessary libraries.
library(scales)
library(quantmod)   # For downloading FRED data
library(dplyr)      # For data manipulation
library(lubridate)  # For working with dates

# Step 1. Get the historical recession indicator from FRED.
# USREC is a series with 1 indicating a recession period.
getSymbols("USREC", src = "FRED")

unrate <- getFRED("unrate", rename_variables = "unemployment")

# Convert the USREC xts object to a data frame and filter for dates since 1940.
recession_df <- data.frame(date = index(USREC), USREC = as.numeric(USREC)) %>%
  filter(date >= as.Date("1940-01-01"))

# Step 2. Ensure your unemployment data frame 'unrate' has a proper Date column.
# Here we assume unrate has columns 'date' and 'unemployment'.
unrate <- unrate %>%
  mutate(date = as.Date(date))

# Step 3. Merge the unemployment data with the recession indicator.
data_combined <- unrate %>%
  left_join(recession_df, by = "date")

# Step 4. Identify the start of each recession.
# A recession starts when USREC changes from 0 (or NA) to 1.
data_combined <- data_combined %>%
  arrange(date) %>%
  mutate(USREC_lag = lag(USREC, default = 0),
         start_of_recession = (USREC_lag == 0 & USREC == 1))

# Step 5. Find the indices in the data where a recession begins.
recession_start_indices <- which(data_combined$start_of_recession)

# Prepare an empty data frame to store recession summaries.
results <- data.frame(
  start_date = as.Date(character()),
  start_unemp = numeric(),
  peak_unemp = numeric(),
  jump = numeric(),
  stringsAsFactors = FALSE
)

# Step 6. Loop over each identified recession start.
# For each recession, we define the “cycle” as extending from the recession start date
# until one day before the next recession starts (or until the end of available data).
for(i in seq_along(recession_start_indices
                   )) {
  start_idx <- recession_start_indices[i]
  start_date <- data_combined$date[start_idx]
  start_unemp <- data_combined$unemployment[start_idx]
  
  # Determine the end of the cycle: day before the next recession start (or end of data)
  if (i < length(recession_start_indices)) {
    end_idx <- recession_start_indices[i + 1] - 1
  } else {
    end_idx <- nrow(data_combined)
  }
  
  period_data <- data_combined[start_idx:end_idx, ]
  
  # Compute the maximum unemployment rate from the recession start up to before the next recession.
  peak_unemp <- max(period_data$unemployment, na.rm = TRUE)
  
  # Calculate the jump in unemployment.
  results <- rbind(results, data.frame(
    start_date = start_date,
    start_unemp = start_unemp,
    peak_unemp = peak_unemp,
    jump = peak_unemp - start_unemp
  ))
}

# Step 7. Display the summary results.
print(results)



results <- as_tibble(results)


# Sort by jump in increasing order so that the smallest jump is at the bottom
# and the largest jump (the last element) is at the top of the plot.
results <- results[order(results$jump, decreasing = FALSE), ]

# Format the date labels as "Month, Year"
results$start_date_formatted <- format(results$start_date, "%B, %Y")
results$start_date_formatted <- factor(results$start_date_formatted, levels = results$start_date_formatted)

results$jump = results$jump/100

# Create the horizontal bar plot with the required modifications
ggplot(results, aes(x = jump, y = start_date_formatted)) +
  geom_col(fill = "#2c3254") +
  geom_text(aes(label = paste0(round(jump * 100, 1), "%")), hjust = -0.2) +
  labs(x = "Increase in Unemployment", 
       y = "",
       title = "Figure 1: No Recession With Unemployment Jumping Less Than 1.9%",
       subtitle = "Unemployment Jump by Start Date",
       caption = "Note 1980 recession moves into 1981 one. Mike Konczal") +
  theme_classic(base_size = 17) +
  scale_x_continuous(label = percent, expand = expansion(mult = c(0, 0.1))) +
  theme(plot.title.position = "plot")