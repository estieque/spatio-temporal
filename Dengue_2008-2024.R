
custom_theme <- theme_minimal(base_size = 36) +  # Larger font
  theme(
    text = element_text(family = "Roboto"),  # Apply Times New Roman
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),  # Title adjustments
    axis.title.x = element_text(size = 32, face = "bold"),
    axis.title.y = element_text(size = 32, face = "bold"),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )



df_corr<- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", sheet = "Sheet1")

data <- df_corr %>%
  dplyr::select(2:6)

summary(data)

data |>
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",
      "{min}, {max}"
      
    ),
    missing = "no"
  )
#################
# Load package
library(e1071)


median(df$Cases)
# Example data
df <- data

# Compute statistics
medians   <- apply(df, 2, median)
skewness_ <- apply(df, 2, skewness)
kurtosis_ <- apply(df, 2, kurtosis)

# Combine into a single data frame
summary_table <- data.frame(
  Variable = names(df),
  Median   = medians,
  Skewness = skewness_,
  Kurtosis = kurtosis_
)

# View result
print(summary_table)

# Save to CSV
write.csv(summary_table, "summary_stats.csv", row.names = FALSE)



##############


df_diar <- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", sheet = "Sheet1")


df_diar$Date <- as.Date(df_diar$Date)

plot_one <- ggplot(df_diar, aes(x = Date, y = Cases)) +
  #geom_point(aes(color = "red"))+
  geom_line(color = "steelblue", size = .8) +  # Line color and size
  labs(x = "Year", y = "Dengue Cases") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, max(95000, na.rm = TRUE), by = 5000))+
  custom_theme+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none")

plot_one

ggsave("dengue/Fig 1.tiff", plot_one,height = 4, width = 6, dpi = 300)



df_diar$Date <- factor(month(df_diar$Date, label = TRUE, abbr = FALSE))  # Full month names

# Boxplot (Panel B) with month names
plot_two<-ggplot(df_diar, aes(x = Date, y = Cases)) +
  geom_boxplot(fill = "#1c9099", color = "#045a8d", outlier.color = "tomato", outlier.shape = 19, outlier.size = 1.2) +  # Boxplot colors
  labs(x = "Months", y = "Dengue Cases") +
  scale_y_continuous(breaks = seq(0, max(95000, na.rm = TRUE), by = 5000))+
custom_theme +  # Apply custom theme with Times New Roman
  theme(
    panel.grid.major = element_line(color = "gray70"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)  # Rotate month names to vertical
  )

plot_two

ggsave("dengue/Fig 2.tiff", plot_two,height = 4, width = 6, dpi = 300)


dengue_cases <- ts(df_diar$Cases, start = c(2008, 1), frequency = 12)

# Perform STL decomposition
decomposition_one <- stl(dengue_cases, s.window = "periodic")

# Save the plot as a TIFF file
tiff("dengue/Fig 3.tiff", height = 4, width = 6, units = "in", res = 300)

# Set plot parameters
par(font.lab = 5,            # Axis labels in bold
    cex.lab = 2.4,           # Font size for axis labels (corrected scale)
    font.axis = 2,           # Axis numbers in italic
    cex.axis = 2,
    mgp = c(3, 1, 0),
    cex.main = 2.4,
    font.main = 5)

# Plot the STL decomposition
plot(decomposition_one, col = "blue")

# Close the device
dev.off()



acf_one <-ggAcf(dengue_cases)+
  ggtitle("Autocorelation for dengue cases") +
  custom_theme +  # Apply custom theme
  theme(panel.grid.major = element_line(color = "gray90")) +
  scale_x_continuous(breaks = seq(0, max(24, na.rm = TRUE), by = 2))

acf_two<-ggPacf(dengue_cases)+
  ggtitle("Partial Autocorelation for dengue cases") +
  custom_theme +  # Apply custom theme
  theme(panel.grid.major = element_line(color = "gray90")) +
  scale_x_continuous(breaks = seq(0, max(24, na.rm = TRUE), by = 2))

combined_plot <- acf_one+acf_two

ggsave("dengue/Fig 4.tiff", combined_plot,height = 4, width = 10, dpi = 300)

###########ARIMA SELECTION########
# Load necessary libraries
library(forecast)
library(tidyverse)


df_diar <- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", sheet = "Sheet1")

# Convert dengue cases to a time series object (monthly data)
dengue_cases <- ts(df_diar$Cases, start = c(2008, 1), frequency = 12)


library(readxl)
library(dplyr)
library(lubridate)

# Load monthly data
df_diar <- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx",
                      sheet = "Sheet1")

# Create a date column assuming monthly sequence starting Jan 2008
df_diar <- df_diar |>
  mutate(
    date = seq(as.Date("2008-01-01"), by = "month", length.out = n()),
    year = year(date)
  )

# Aggregate to yearly totals
df_yearly <- df_diar |>
  group_by(year) |>
  summarise(
    total_cases = sum(Cases, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to time series object (start year = first year in data)
dengue_yearly_ts <- ts(df_yearly$total_cases, start = c(2008,1), frequency = 12)

# Optional: preview
print(dengue_yearly_ts)


SARIMA(2,1,1)(1,1,1)[6]
SARIMA(2,1,2)(1,1,1)[6]
SARIMA(2,1,2)(2,1,2)[6]




# Fit the ARIMA model with seasonal components (period = 6)
d.arima <- Arima(dengue_cases, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 6))
d.arima <- Arima(dengue_cases, order = c(1,1,2), seasonal = list(order = c(1,1,1), period = 6))
d.arima <- Arima(dengue_cases, order = c(2,1,1), seasonal = list(order = c(1,1,1), period = 6))
d.arima <- Arima(dengue_cases, order = c(2,1,2), seasonal = list(order = c(1,1,1), period = 6))


d.arima <- Arima(dengue_cases, order = c(2,1,2), seasonal = list(order = c(2,1,2), period = 6))

d.arima <- Arima(dengue_cases, order = c(1,1,2), seasonal = list(order = c(1,1,1), period = 6))
d.arima <- Arima(dengue_cases, order = c(0,1,0), seasonal = list(order = c(1,1,1), period = 6))


summary(d.arima)
Box.test(d.arima$residuals, , lag = 6, type = "Ljung-Box")
# Forecast the next 50 periods
d.forecast <- forecast(d.arima, level = c(95), h = 36)


# Plot the forecast

arima <- autoplot(d.forecast) +
  ggtitle("SARIMA(2,1,2)(1,1,1)[6] Forecast") +
  labs(x = "Year", y = "Dengue Cases") +
  scale_x_continuous(breaks = seq(min(2008), max(2034), by = 1)) + 
  #custom_theme +  # Apply custom theme
  theme(panel.grid.major = element_line(color = "gray80"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

arima

# Plot with log-scaled y-axis and better labels
arima_plot <- autoplot(d.forecast) +
  scale_y_log10(
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_x_continuous(
    breaks = seq(2008, 2035, by = 1)
  ) +
  labs(
    title = "ARIMA Model",
    x = "Years",
    y = "Number of dengue cases"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

arima_plot


autoplot(d.forecast) +
  scale_y_continuous(
    trans = log10_trans(),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(
    title = "ARIMA Model",
    x = "Years",
    y = "Number of dengue cases"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

arima

ggsave("ARIMA 2008-2024.png",arima,width = 18, height = 10, dpi = 300)

####################################

# Load required libraries
library(readxl)
library(forecast)
library(ggplot2)
library(scales)

# Load monthly dengue data
df_diar <- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", 
                      sheet = "Sheet1")

# Step 1: Add 1 to avoid zeros
df_diar$adjusted_cases <- df_diar$Cases + 1

# Step 2: Log-transform the series (natural log)
df_diar$log_cases <- log(df_diar$adjusted_cases)

# Step 3: Convert to time series object (monthly data)
log_cases_ts <- ts(df_diar$`LN Cases`, start = c(2008, 1), frequency = 12)

# Step 4: Fit SARIMA model on log-transformed data
d.arima <- Arima(log_cases_ts, order = c(2, 1, 1), 
                 seasonal = list(order = c(1, 1, 1), period = 6))

# Optional: Diagnostic test
Box.test(d.arima$residuals, lag = 6, type = "Ljung-Box")

# Step 5: Forecast 36 months ahead (3 years)
d.forecast <- forecast(d.arima, level = 95, h = 36)

# Step 6: Back-transform forecast to original scale (exp then subtract 1)
d.forecast$mean  <- exp(d.forecast$mean) - 1
d.forecast$lower <- exp(d.forecast$lower) - 1
d.forecast$upper <- exp(d.forecast$upper) - 1

# Step 7: Plot forecast in original scale with log-scaled Y-axis
final_plot <- autoplot(d.forecast) +
  scale_y_continuous(
    trans = log10_trans(),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(
    title = "SARIMA(2,1,1)(1,1,1)[6] Forecast",
    #subtitle = "Back-transformed from log scale (exp(log_cases) - 1)",
    x = "Year",
    y = "Number of dengue cases"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

# Display the plot
final_plot

# Step 8: Export the plot
ggsave("sarima_forecast_2008_2027_logscale.png", plot = final_plot, 
       width = 12, height = 6, dpi = 300)

########################
# Use original values (no log)
cases_ts <- ts(df_diar$`Cases + 1`, start = c(2008, 1), frequency = 12)

# Fit SARIMA
fit <- Arima(cases_ts, order = c(2,1,1), seasonal = list(order = c(1,1,1), period = 6))

# Forecast
fcast <- forecast(fit, h = 36, level = 95)

# Plot with log10 y-axis
autoplot(fcast) +
  scale_y_continuous(
    trans = log10_trans(),
    labels = trans_format("log10", math_format(10^.x)),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  labs(
    title = "ARIMA Model Forecast",
    x = "Years",
    y = "Number of dengue cases"
  ) +
  custom_theme


# Forecast is in log scale, so back-transform
fcast <- forecast(d.arima, h = 36, level = 95)

# Back-transform
fcast$mean  <- exp(fcast$mean) - 1
fcast$lower <- exp(fcast$lower) - 1
fcast$upper <- exp(fcast$upper) - 1

# Plot on log10 scale
autoplot(fcast) +
  scale_y_continuous(
    trans = log10_trans(),
    labels = trans_format("log10", math_format(10^.x)),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  labs(
    title = "ARIMA Model Forecast",
    x = "Years",
    y = "Number of dengue cases"
  ) +
  theme_minimal()


#############
library(forecast)
library(scales)
library(ggplot2)

# Create log-transformed time series
log_cases_ts <- ts(log(df_diar$`Cases` + 1), start = c(2008, 1), frequency = 12)

# Fit ARIMA model on log-transformed data
d.arima <- Arima(log_cases_ts, order = c(2,1,1), seasonal = list(order = c(1,1,1), period = 6))

# Forecast
fcast <- forecast(d.arima, h = 36, level = 95)

# Back-transform forecast
fcast$mean  <- exp(fcast$mean) - 1
fcast$lower <- exp(fcast$lower) - 1
fcast$upper <- exp(fcast$upper) - 1

# Back-transform original series
original_backtrans <- exp(log_cases_ts) - 1

# Plot original + forecast on log10 scale
autoplot(fcast) +
  autolayer(original_backtrans, series = "Observed", color = "black") +
  scale_y_continuous(
    trans = log10_trans(),
    labels = trans_format("log10", math_format(10^.x)),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  labs(
    title = "ARIMA Model Forecast",
    x = "Years",
    y = "Number of dengue cases"
  ) +
  theme_minimal()

library(forecast)
library(scales)
library(ggplot2)

df_diar <- read_excel("H:\\downloads_2025\\fmd new.xlsx", sheet = "Sheet11")
# Create log-transformed time series
log_cases_ts <- ts(log(df_diar$FMD_Cases + 1), start = c(2017, 1), frequency = 12)

log_cases_ts <- ts((df_diar$FMD_Cases + 1), start = c(2017, 1), frequency = 12)
# Fit SARIMA on log scale
d.arima <- Arima(log_cases_ts, order = c(2,1,2), seasonal = list(order = c(1,0,1), period = 6))

summary(d.arima)
# Forecast
fcast <- forecast(d.arima, h = 36, level = 95)

# Back-transform forecast (mean only)
fcast_bt <- ts(exp(fcast$mean) - 1, start = end(log_cases_ts) + c(0, 1), frequency = 12)

# Back-transform observed series
observed_bt <- ts(exp(log_cases_ts) - 1, start = start(log_cases_ts), frequency = 12)

# Plot only observed + forecast mean (no intervals)
arima <- autoplot(observed_bt, series = "Observed", size=2) +
  autolayer(fcast_bt, series = "Forecast", color = "blue",size=1.3) +
  scale_y_continuous(
    trans = log10_trans(),
    labels = trans_format("log10", math_format(10^.x)),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  scale_color_manual(values = c("Observed" = "black", "Forecast" = "blue")) +
  theme_classic(base_family = "Roboto")+  # Larger font
  theme(
    text = element_text(family = "Roboto"),  # Apply Times New Roman
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),  # Title adjustments
    axis.title.x = element_text(size = 38, face = "bold"),
    axis.title.y = element_text(size = 38, face = "bold"),
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.ticks.length=unit(.35, "cm"),
    axis.line = element_line(colour = "black", 
                             size = 1, linetype = "solid")
  )+
  theme(legend.position = "none",
        #panel.grid.major = element_line(color = "gray80"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_x_continuous(
    breaks = seq(2017, 2035, by = 1)
  ) +
  labs(
    title = "ARIMA Model",
    x = "Years",
    y = "Number of dengue cases"
  ) + geom_vline(xintercept = 2024, linetype = "dashed", color = "grey20", linewidth = 1.2)+
  annotate("text",
           x = 2024,
           y = 10^1,  # Bottom of plot
           label = "Forecast Start",
           angle = 0,        # Horizontal text
           #vjust = -1.5,     # Push above x-axis
           hjust = -0.08,
           size = 5,
           fontface = "bold",
           color = "black")

###FMD##

arima <- autoplot(observed_bt, series = "Observed", size=2) +
  autolayer(fcast_bt, series = "Forecast", color = "blue", size=1.3) +
  scale_y_continuous(
    trans = log10_trans(),
    labels = trans_format("log10", math_format(10^.x)),
    breaks = trans_breaks("log10", function(x) 10^x),
    limits = c(1e3, NA)  # Set lower y-axis limit
  ) +
  scale_color_manual(values = c("Observed" = "black", "Forecast" = "blue")) +
  theme_classic(base_family = "Roboto") +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
    axis.title.x = element_text(size = 38, face = "bold"),
    axis.title.y = element_text(size = 38, face = "bold"),
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.ticks.length = unit(.35, "cm"),
    axis.line = element_line(colour = "black", size = 1)
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  scale_x_continuous(
    breaks = seq(2017, 2035, by = 1)
  ) +
  labs(
    title = "ARIMA Model",
    x = "Years",
    y = "Number of FMD cases"
  ) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "grey20", linewidth = 1.2) +
  annotate("text",
           x = 2024,
           y = 10^3.5,  # Match bottom of visible range
           label = "Forecast Start",
           hjust = -0.08,
           size = 5,
           fontface = "bold",
           color = "black")


arima
ggsave("ARIMA 2017-2024.tiff",arima,width = 15, height = 8, dpi = 800)


############

#######################

#####################################


###########

library(forecast)
library(ggplot2)
library(scales)

# Convert to time series
dengue_yearly_ts <- ts(df_yearly$total_cases, start = 2008, frequency = 1)

# Fit ARIMA model (no seasonality)
d.arima <- Arima(dengue_yearly_ts, order = c(2, 1, 2), lambda = "auto")

# Check residuals
Box.test(d.arima$residuals, lag = 6, type = "Ljung-Box")

# Forecast next 10 years (you can change h = 4 to h = 10 if needed)
d.forecast <- forecast(d.arima, level = 95, h = 10)

# Plot with log-scaled y-axis and better labels
arima_plot <- autoplot(d.forecast) +
  scale_y_log10(
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_x_continuous(
    breaks = seq(2008, 2035, by = 1)
  ) +
  labs(
    title = "ARIMA Model",
    x = "Years",
    y = "Number of dengue cases"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

arima_plot



###################




df_corr<- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", sheet = "Sheet1")

df_corr <- df_corr %>%
  select(2:6)

colnames(df_corr) <- c("Dengue Cases","Temperature (°C)", "Precipitation (mm)", "Humidity (%)",  "Wind Speed (m/s)")


head(df_corr)


res1 <- cor.mtest(df_corr, conf.level = .95)

tiff("dengue/Fig 7.tiff", width = 7, height = 6, units = "in", res = 1400)

corrplot(corr = cor(df_corr, method = "spearman"),
         #xlab = c("Diarhoea in Children", "Max Temperature", "Precipitation","Drought Index"),
         addCoef.col = "white",
         outline = "white",
         cl.length = 10,
         number.cex = 1.2,
         method= "color",
         type = "lower",
         tl.pos = "ld",
         tl.cex = 1.5,
         order = "original",
         tl.col = "black",
         p.mat = res1$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 2,
         pch.col = "#25ec00",
         col = COL1('Greys'),
)
dev.off()

data<- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", sheet = "Sheet1")

Mean_temp <- data$Mean_temp
Precipitation <- data$Precipitation
Humidity <- data$Humidity
Wind_speed <- data$Wind_speed

# Create lagged variables (e.g., lag of 1 month and 2 months for temperature and humidity)
data_lagged_one <- data %>%
  mutate(Mean_temp_lag1 = lag(Mean_temp, 1),
         Precipitation_lag1 = lag(Precipitation, 1),
         Humidity_lag1 = lag(Humidity, 1),
         Wind_speed_lag1 = lag(Wind_speed, 1))


# Remove rows with NA values caused by lagging
data_lagged_one <- na.omit(data_lagged_one)


####correlation with lagged one month####
data_lagged_one_corr<- data_lagged_one %>% 
  select(Cases, 9:12)



library(car)



# Fit Poisson regression model with centered variables
model <- glm(Cases ~ .,
             family = poisson(link = "log"), data = data_lagged_one_corr)

# Run VIF test
vif_values <- vif(model)

# Print the VIFs
print(vif_values)


colnames(data_lagged_one_corr) <- c("Dengue Cases","Temperature (°C) lag1", "Precipitation (mm) lag1", "Humidity (%) lag1",  "Wind Speed (m/s) lag1")

res1 <- cor.mtest(data_lagged_one_corr, conf.level = .95)
tiff("dengue/Fig 8 (a).tiff", width = 7, height = 6, units = "in", res = 1400)

corrplot(corr = cor(data_lagged_one_corr, method = "spearman"),
                #xlab = c("Diarhoea in Children", "Max Temperature", "Precipitation","Drought Index"),
                addCoef.col = "white",
                outline = "black",
                cl.length = 10,
                number.cex = 1.2,
                method= "color",
                type = "lower",
                tl.pos = "ld",
                tl.cex = 1.5,
                order = "original",
                tl.col = "black",
                p.mat = res1$p,
                insig = "label_sig",
                sig.level = c(.001, .01, .05),
                pch.cex = 2,
                pch.col = "#25ec00",
                col = COL1('Greys'),
)
dev.off()


# Fit Poisson model using lagged variables
model_one <- glm(Cases ~ Mean_temp_lag1 + Precipitation_lag1 + Humidity_lag1+Wind_speed_lag1,
                 family = poisson(), data = data_lagged_one)
reg_one<-tbl_regression(model_one,
                        exponentiate = F)%>%
  modify_header(  label = "**Variables**",
                  stat_n = "**N**",
                  estimate = "**IRR**",
                  p.value = "**P-value**") %>%
  #add_global_p() %>%
  bold_labels()%>%
  bold_p(t=0.05) %>%
  bold_labels() %>%
  italicize_labels()



# Create lagged variables (e.g., lag of 1 month and 2 months for temperature and humidity)
data_lagged_two <- data %>%
  mutate(Mean_temp_lag2 = lag(Mean_temp, 2),
         Precipitation_lag2 = lag(Precipitation, 2),
         Humidity_lag2 = lag(Humidity, 2),
         Wind_speed_lag2 = lag(Wind_speed, 2))


# Remove rows with NA values caused by lagging
data_lagged_two <- na.omit(data_lagged_two)


####correlation with lagged one month####
data_lagged_two_corr<- data_lagged_two %>% 
  select(Cases, 9:12)
model <- glm(Cases ~ .,
             family = poisson(link = "log"), data = data_lagged_one_corr)

# Run VIF test
vif_values <- vif(model)

# Print the VIFs
print(vif_values)
colnames(data_lagged_two_corr) <- c("Dengue Cases","Temperature (°C) lag2", "Precipitation (mm) lag2", "Humidity (%) lag2",  "Wind Speed (m/s) lag2")

res1 <- cor.mtest(data_lagged_two_corr, conf.level = .95)
tiff("dengue/Fig 8 (b).tiff", width = 7, height = 6, units = "in", res = 1200)

corrplot(corr = cor(data_lagged_two_corr, method = "spearman"),
         #xlab = c("Diarhoea in Children", "Max Temperature", "Precipitation","Drought Index"),
         addCoef.col = "white",
         outline = "black",
         cl.length = 10,
         number.cex = 1.2,
         method= "color",
         type = "lower",
         tl.pos = "ld",
         tl.cex = 1.5,
         order = "original",
         tl.col = "black",
         p.mat = res1$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 2,
         pch.col = "#25ec00",
         col = COL1('Greys'),
)
dev.off()





poisson_model_fmd <- glm(`Dengue Cases`~ ., family = poisson(), data = df_corr)
summary(poisson_model_fmd)


tbl_regression(poisson_model_fmd,
               exponentiate = T)%>%
  modify_header(  label = "**Variables**",
                  stat_n = "**N**",
                  estimate = "**IRR**",
                  p.value = "**P-value**") %>%
  #add_global_p() %>%
  bold_labels()%>%
  bold_p(t=0.05) %>%
  bold_labels() %>%
  italicize_labels()
###########
# Load required package
install.packages("Metrics")  # Run this once if not installed
library(Metrics)

# STEP 1: Split the data into training and testing sets
set.seed(123)  # For reproducibility
sample_index <- sample(1:nrow(df_corr), size = 0.8 * nrow(df_corr))
train_data <- df_corr[sample_index, ]
test_data <- df_corr[-sample_index, ]

# STEP 2: Fit Poisson regression model
poisson_model <- glm(`Dengue Cases` ~ ., family = poisson(), data = train_data)

# STEP 3: Predict on the test data (use type = "response" to get counts)
predicted_counts <- predict(poisson_model, newdata = test_data, type = "response")

# STEP 4: Extract actual dengue case counts from test set
actual_counts <- test_data$`Dengue Cases`

# STEP 5: Calculate RMSE (manually and with package)
rmse_manual <- sqrt(mean((predicted_counts - actual_counts)^2))
rmse_package <- rmse(actual_counts, predicted_counts)

# STEP 6: Print results
cat("RMSE (manual):", rmse_manual, "\n")
cat("RMSE (Metrics package):", rmse_package, "\n")

############





# Fit Poisson model using lagged variables
model_two <- glm(Cases ~ Mean_temp_lag2 + Precipitation_lag2 + Humidity_lag2+Wind_speed_lag2,
                 family = poisson(), data = data_lagged_two)
reg_two<-tbl_regression(model_two,
                        exponentiate = F)%>%
  modify_header(  label = "**Variables**",
                  stat_n = "**N**",
                  estimate = "**IRR**",
                  p.value = "**P-value**") %>%
  #add_global_p() %>%
  bold_labels()%>%
  bold_p(t=0.05) %>%
  bold_labels() %>%
  italicize_labels()

final_regression_table <-
  tbl_merge(tbls = list(reg_one, reg_two),
            tab_spanner = c("**Poisson regression with one month Lag**", "**Poisson regression with two month Lag**"))%>%
  as_gt() %>%
  fmt_number(
    decimals = 3
  )%>%
  gtsave("regression_table_for_publication2_ 2008-2024.docx")


######## VIF######



model_vif <- lm(`Cases`~ ., data = data)

library(car)

# Calculating VIF
vif_values <- vif(model_vif)
vif_values

vif <- as.data.frame(vif_values)

colnames(vif) <- c("Variables", "vif_values")


df_diar <- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", sheet = "Sheet1")
Mean_temp <- ts(df_diar$Mean_temp, start = c(2008, 1), frequency = 12)
Precipitation <- ts(df_diar$Precipitation, start = c(2008, 1), frequency = 12)
Humidity<- ts(df_diar$Humidity, start = c(2008, 1), frequency = 12)
Wind_speed <- ts(df_diar$Wind_speed, start = c(2008, 1), frequency = 12)

temp_ <- ggCcf(
  dengue_cases,
  Mean_temp,
  lag.max = 12,
  type = "correlation"
  #plot = TRUE,
  #na.action = na.contiguous
)+
  ggtitle("Dengue cases and Temperature (°C)") +
  theme_classic(base_family = "Roboto") +  # Apply custom theme
  #theme(panel.grid.major = element_line(color = "gray90")) +
  scale_x_continuous(breaks = seq(0, max(24, na.rm = TRUE), by = 2))+  # Larger font
  theme(
    text = element_text(family = "Roboto"),  # Apply Times New Roman
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),  # Title adjustments
    axis.title.x = element_text(size = 38, face = "bold"),
    axis.title.y = element_text(size = 38, face = "bold"),
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.ticks.length=unit(.35, "cm"),
    axis.line = element_line(colour = "black", 
                             size = 1, linetype = "solid")
  )


Precipitation_ <- ggCcf(
  dengue_cases,
  Precipitation,
  lag.max = 12,
  type = "correlation"
  #plot = TRUE,
  #na.action = na.contiguous
)+
  ggtitle("Dengue cases and Precipitation (mm)") +
  theme_classic(base_family = "Roboto") +  # Apply custom theme
  #theme(panel.grid.major = element_line(color = "gray90")) +
  scale_x_continuous(breaks = seq(0, max(24, na.rm = TRUE), by = 2))+  # Larger font
  theme(
    text = element_text(family = "Roboto"),  # Apply Times New Roman
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),  # Title adjustments
    axis.title.x = element_text(size = 38, face = "bold"),
    axis.title.y = element_text(size = 38, face = "bold"),
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.ticks.length=unit(.35, "cm"),
    axis.line = element_line(colour = "black", 
                             size = 1, linetype = "solid")
  )



Humidity_ <- ggCcf(
  dengue_cases,
  Humidity,
  lag.max = 12,
  type = "correlation"
  #plot = TRUE,
  #na.action = na.contiguous
)+
  ggtitle("Dengue cases and Humidity (%)") +
  theme_classic(base_family = "Roboto") +  # Apply custom theme
  #theme(panel.grid.major = element_line(color = "gray90")) +
  scale_x_continuous(breaks = seq(0, max(24, na.rm = TRUE), by = 2))+  # Larger font
  theme(
    text = element_text(family = "Roboto"),  # Apply Times New Roman
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),  # Title adjustments
    axis.title.x = element_text(size = 38, face = "bold"),
    axis.title.y = element_text(size = 38, face = "bold"),
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.ticks.length=unit(.35, "cm"),
    axis.line = element_line(colour = "black", 
                             size = 1, linetype = "solid")
  )


Wind_speed_ <- ggCcf(
  dengue_cases,
  Wind_speed,
  lag.max = 12,
  type = "correlation"
  #plot = TRUE,
  #na.action = na.contiguous
)+
  ggtitle("Dengue cases and Wind Speed (m/s)") +
  theme_classic(base_family = "Roboto") +  # Apply custom theme
  #theme(panel.grid.major = element_line(color = "gray90")) +
  scale_x_continuous(breaks = seq(0, max(24, na.rm = TRUE), by = 2))+  # Larger font
  theme(
    text = element_text(family = "Roboto"),  # Apply Times New Roman
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),  # Title adjustments
    axis.title.x = element_text(size = 38, face = "bold"),
    axis.title.y = element_text(size = 38, face = "bold"),
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.ticks.length=unit(.35, "cm"),
    axis.line = element_line(colour = "black", 
                             size = 1, linetype = "solid")
  )

ccf_plot <- temp_+ Precipitation_ + Humidity_+Wind_speed_

ggsave("dengue/Fig 6.tiff", ccf_plot,height = 12, width = 18, dpi = 1200)



########### box cox transformation #####

# Load packages
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(tseries)

# Step 1: Load and clean data
df_diar <- read_excel("H:\\dengue\\Updated version_02.01.2025\\Supplementary Materials.xlsx", sheet = "Sheet1") %>%
  select(2) %>%
  rename(Cases = 1) %>%
  filter(Cases != 0)

# Step 2: Convert to time series
dengue_cases <- ts(df_diar$Cases, start = c(2008, 1), frequency = 12)

# Step 3: Train/Test Split (last 24 months for testing)
n <- length(dengue_cases)
train_ts <- window(dengue_cases, end = c(2024, 12)) # training up to 2021
test_ts <- window(dengue_cases, start = c(2022, 1)) # testing from 2022

# Step 4: Box-Cox Transformation
lambda <- BoxCox.lambda(train_ts)
train_boxcox <- BoxCox(train_ts, lambda)

# Step 5: Fit SARIMA model
model <- auto.arima(train_boxcox, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, lambda = NULL)

d.arima <- Arima(train_boxcox, order = c(2,1,2), seasonal = list(order = c(1,1,1), period = 12))
# Step 6: Forecast
h <- length(test_ts)
forecast_boxcox <- forecast(model, h = h)

# Step 7: Inverse transform
forecast_mean <- InvBoxCox(forecast_boxcox$mean, lambda)
forecast_lower <- pmax(InvBoxCox(forecast_boxcox$lower[,2], lambda), 0)  # 95% lower
forecast_upper <- InvBoxCox(forecast_boxcox$upper[,2], lambda)

# Step 8: Combine actual and forecast for plotting
actual_vals <- ts(dengue_cases, start = c(2008, 1), frequency = 12)
forecast_vals <- ts(forecast_mean, start = c(2022, 1), frequency = 12)

# Step 9: Plot
df_plot <- data.frame(
  Date = c(time(actual_vals), time(forecast_vals)),
  Cases = c(as.numeric(actual_vals), as.numeric(forecast_vals)),
  Type = c(rep("Actual", length(actual_vals)), rep("Forecast", length(forecast_vals)))
)

df_ci <- data.frame(
  Date = time(forecast_vals),
  lower = forecast_lower,
  upper = forecast_upper
)

ggplot(df_plot, aes(x = Date, y = Cases, color = Type)) +
  geom_line(size = 1) +
  geom_ribbon(data = df_ci, aes(x = Date, ymin = lower, ymax = upper),
              fill = "blue", alpha = 0.3, inherit.aes = FALSE) +
  labs(title = paste("SARIMA Forecast (Lambda =", round(lambda, 2), ")"),
       x = "Year", y = "Dengue Cases") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()


library(forecast)

d.arima <- Arima(dengue_yearly_ts, order = c(2,1,2), lambda = "auto")


# Load required library
library(forecast)

# Ensure your dengue case data is a numeric time series
y <- ts(dengue_cases)  # Replace with your actual vector

# Prepare the climatic predictors as a numeric matrix
xreg <- as.matrix(df_diar[, c("Mean_temp", "Precipitation", "Wind_speed", "Humidity")])

# Fit ARIMA(2,1,2) with regressors
fit_arimax <- arima(y, order = c(2, 1, 2), xreg = xreg)
summary(fit_arimax)

# Install if not already
install.packages("lmtest")
library(lmtest)

coeftest(fit_arimax)

####### xg-boost for climate predictor####

# 📦 Install necessary packages if not already installed
if (!require("xgboost")) install.packages("xgboost")
if (!require("caret")) install.packages("caret")
if (!require("Metrics")) install.packages("Metrics")

# 📚 Load libraries
library(xgboost)
library(caret)
library(Metrics)

# 📄 Your data (assuming df_diar contains the following columns)
# Columns: Dengue_cases, Mean_temp, Precipitation, Wind_speed, Humidity

# ✅ Prepare data
df <- df_diar %>%
  select(2:6)
df <- na.omit(df)  # Remove NA values if any

# Predictor matrix (x) and target vector (y)
x <- as.matrix(df[, c("Mean_temp", "Precipitation", "Wind_speed", "Humidity")])
y <- df$Cases

# ✅ Train/test split (80/20) — no shuffle because it's time series
n <- nrow(df)
train_index <- 1:floor(0.8 * n)
x_train <- x[train_index, ]
y_train <- y[train_index]
x_test <- x[-train_index, ]
y_test <- y[-train_index]

# ✅ Convert to DMatrix (required for xgboost)
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# ✅ Train XGBoost regression model
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0
)

# ✅ Make predictions
preds <- predict(model, dtest)

# ✅ Calculate RMSE
rmse_value <- rmse(y_test, preds)
cat("✅ RMSE:", round(rmse_value, 2), "\n")

# ✅ Feature importance
importance_matrix <- xgb.importance(model = model)
print(importance_matrix)

# ✅ Plot feature importance
xgb.plot.importance(importance_matrix, top_n = 10)



########merge image ####


# Read the two TIFF files
img1 <- image_read("H:\\dengue\\revision two\\supplimentary\\figure 5 (a).tiff")
img2 <- image_read("H:\\dengue\\revision two\\supplimentary\\figure 5 (b).tiff")

img2 <- image_resize(img2, geometry_size_pixels(width = image_info(img1)$width))

# Combine side-by-side (horizontal merge)
merged_h <- image_append(c(img1, img2), stack = FALSE)

# Combine top-to-bottom (vertical merge)
merged_v <- image_append(c(img1, img2), stack = TRUE)

# Save the merged image
image_write(merged_h, "merged_horizontal.tiff", format = "tiff")
image_write(merged_v, "merged_vertical.tiff", density = "1200x1200",format = "tiff")


# Set DPI explicitly
merged_img <- image_density(merged_v, "1200x1200")  # Set 300 DPI

# Save the output TIFF
image_write(merged_img, path = "merged_high_dpi.tiff", density = "1200x1200", format = "tiff")



