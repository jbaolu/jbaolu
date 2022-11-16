library(tidyverse)
library(AER)
library(MASS)
library(moments)
library(readxl)
library(ggstatsplot)
library(scales)
library(ggcorrplot)

#import the dataset and select a sample
financial_data <- read_excel("C:/GIA BUU/YUAN ZE/Courses/CM 506 - Econometrics/data/financial_data_16Q4_22Q2 (3).xlsx")

#count for the data
number_of_firm <- financial_data %>%
  group_by(date, sector) %>%
  summarise(num_of_firms = n())

view(number_of_firm)

# compute sample averages
avg_roe <- mean(financial_data$roe)
avg_gm <- mean(financial_data$gm)
avg_op_exp_ratio <- mean(financial_data$op_exp_ratio)
avg_cf_ratio <- mean(financial_data$cf_ratio)
avg_debt_cost <- mean(financial_data$debt_cost)
avg_current_ratio <- mean(financial_data$current_ratio)
avg_exp_sales_ratio <- mean(financial_data$exp_sales_ratio)
avg_liab_equity_ratio <- mean(financial_data$liab_equity_ratio)
avg_ltcap_fixedasset_ratio <- mean(financial_data$ltcap_fixedasset_ratio)
avg_equity_asset_ratio <- mean(financial_data$equity_asset_ratio)
avg_asset_turnover <- mean(financial_data$asset_turnover)
avg_int_sales_ratio <- mean(financial_data$int_sales_ratio)
avg_receiv_sales_ratio <- mean(financial_data$receiv_sales_ratio)
avg_pe <- mean(financial_data$pe, na.rm = TRUE)
avg_pb <- mean(financial_data$pb, na.rm = TRUE)
avg_ps <- mean(financial_data$ps, na.rm = TRUE)

#compute standard deviation
sd_roe <- sd(financial_data$roe)
sd_gm <- sd(financial_data$gm)
sd_op_exp_ratio <- sd(financial_data$op_exp_ratio)
sd_cf_ratio <- sd(financial_data$cf_ratio)
sd_debt_cost <- sd(financial_data$debt_cost)
sd_current_ratio <- sd(financial_data$current_ratio)
sd_exp_sales_ratio <- sd(financial_data$exp_sales_ratio)
sd_liab_equity_ratio <- sd(financial_data$liab_equity_ratio)
sd_ltcap_fixedasset_ratio <- sd(financial_data$ltcap_fixedasset_ratio)
sd_equity_asset_ratio <- sd(financial_data$equity_asset_ratio)
sd_asset_turnover <- sd(financial_data$asset_turnover)
sd_int_sales_ratio <- sd(financial_data$int_sales_ratio)
sd_receiv_sales_ratio <- sd(financial_data$receiv_sales_ratio)
sd_pe <- sd(financial_data$pe, na.rm = TRUE)
sd_pb <- sd(financial_data$pb, na.rm = TRUE)
sd_ps <- sd(financial_data$ps, na.rm = TRUE)

#compute median
median_roe <- median(financial_data$roe)
median_gm <- median(financial_data$gm)
median_op_exp_ratio <- median(financial_data$op_exp_ratio)
median_cf_ratio <- median(financial_data$cf_ratio)
median_debt_cost <- median(financial_data$debt_cost)
median_current_ratio <- median(financial_data$current_ratio)
median_exp_sales_ratio <- median(financial_data$exp_sales_ratio)
median_liab_equity_ratio <- median(financial_data$liab_equity_ratio)
median_ltcap_fixedasset_ratio <- median(financial_data$ltcap_fixedasset_ratio)
median_equity_asset_ratio <- median(financial_data$equity_asset_ratio)
median_asset_turnover <- median(financial_data$asset_turnover)
median_int_sales_ratio <- median(financial_data$int_sales_ratio)
median_receiv_sales_ratio <- median(financial_data$receiv_sales_ratio)
median_pe <- median(financial_data$pe, na.rm = TRUE)
median_pb <- median(financial_data$pb, na.rm = TRUE)
median_ps <- median(financial_data$ps, na.rm = TRUE)


#compute min
min_roe <- min(financial_data$roe)
min_gm <- min(financial_data$gm)
min_op_exp_ratio <- min(financial_data$op_exp_ratio)
min_cf_ratio <- min(financial_data$cf_ratio)
min_debt_cost <- min(financial_data$debt_cost)
min_current_ratio <- min(financial_data$current_ratio)
min_exp_sales_ratio <- min(financial_data$exp_sales_ratio)
min_liab_equity_ratio <- min(financial_data$liab_equity_ratio)
min_ltcap_fixedasset_ratio <- min(financial_data$ltcap_fixedasset_ratio)
min_equity_asset_ratio <- min(financial_data$equity_asset_ratio)
min_asset_turnover <- min(financial_data$asset_turnover)
min_int_sales_ratio <- min(financial_data$int_sales_ratio)
min_receiv_sales_ratio <- min(financial_data$receiv_sales_ratio)
min_pe <- min(financial_data$pe, na.rm = TRUE)
min_pb <- min(financial_data$pb, na.rm = TRUE)
min_ps <- min(financial_data$ps, na.rm = TRUE)


#compute max
max_roe <- max(financial_data$roe)
max_gm <- max(financial_data$gm)
max_op_exp_ratio <- max(financial_data$op_exp_ratio)
max_cf_ratio <- max(financial_data$cf_ratio)
max_debt_cost <- max(financial_data$debt_cost)
max_current_ratio <- max(financial_data$current_ratio)
max_exp_sales_ratio <- max(financial_data$exp_sales_ratio)
max_liab_equity_ratio <- max(financial_data$liab_equity_ratio)
max_ltcap_fixedasset_ratio <- max(financial_data$ltcap_fixedasset_ratio)
max_equity_asset_ratio <- max(financial_data$equity_asset_ratio)
max_asset_turnover <- max(financial_data$asset_turnover)
max_int_sales_ratio <- max(financial_data$int_sales_ratio)
max_receiv_sales_ratio <- max(financial_data$receiv_sales_ratio)
max_pe <- max(financial_data$pe, na.rm = TRUE)
max_pb <- max(financial_data$pb, na.rm = TRUE)
max_ps <- max(financial_data$ps, na.rm = TRUE)


#set up a vector of percentiles (25% and 75%) and compute the quantiles 
quantiles <- c(0.25, 0.75)

q_roe <- quantile(financial_data$roe, quantiles)
q_gm <- quantile(financial_data$gm, quantiles)
q_op_exp_ratio <- quantile(financial_data$op_exp_ratio, quantiles)
q_cf_ratio <- quantile(financial_data$cf_ratio, quantiles)
q_debt_cost <- quantile(financial_data$debt_cost, quantiles)
q_current_ratio <- quantile(financial_data$current_ratio, quantiles)
q_exp_sales_ratio <- quantile(financial_data$exp_sales_ratio, quantiles)
q_liab_equity_ratio <- quantile(financial_data$liab_equity_ratio, quantiles)
q_ltcap_fixedasset_ratio <- quantile(financial_data$ltcap_fixedasset_ratio, quantiles)
q_equity_asset_ratio <- quantile(financial_data$equity_asset_ratio, quantiles)
q_asset_turnover <- quantile(financial_data$asset_turnover, quantiles)
q_int_sales_ratio <- quantile(financial_data$int_sales_ratio, quantiles)
q_receiv_sales_ratio <- quantile(financial_data$receiv_sales_ratio, quantiles)
q_pe <- quantile(financial_data$pe, quantiles, na.rm = TRUE)
q_pb <- quantile(financial_data$pb, quantiles, na.rm = TRUE)
q_ps <- quantile(financial_data$ps, quantiles, na.rm = TRUE)


#compute skewness
sk_roe <- skewness(financial_data$roe)
sk_gm <- skewness(financial_data$gm)
sk_op_exp_ratio <- skewness(financial_data$op_exp_ratio)
sk_cf_ratio <- skewness(financial_data$cf_ratio)
sk_debt_cost <- skewness(financial_data$debt_cost)
sk_current_ratio <- skewness(financial_data$current_ratio)
sk_exp_sales_ratio <- skewness(financial_data$exp_sales_ratio)
sk_liab_equity_ratio <- skewness(financial_data$liab_equity_ratio)
sk_ltcap_fixedasset_ratio <- skewness(financial_data$ltcap_fixedasset_ratio)
sk_equity_asset_ratio <- skewness(financial_data$equity_asset_ratio)
sk_asset_turnover <- skewness(financial_data$asset_turnover)
sk_int_sales_ratio <- skewness(financial_data$int_sales_ratio)
sk_receiv_sales_ratio <- skewness(financial_data$receiv_sales_ratio)
sk_pe <- skewness(financial_data$pe, na.rm = TRUE)
sk_pb <- skewness(financial_data$pb, na.rm = TRUE)
sk_ps <- skewness(financial_data$ps, na.rm = TRUE)


#compute kurtosis
k_roe <- kurtosis(financial_data$roe)
k_gm <- kurtosis(financial_data$gm)
k_op_exp_ratio <- kurtosis(financial_data$op_exp_ratio)
k_cf_ratio <- kurtosis(financial_data$cf_ratio)
k_debt_cost <- kurtosis(financial_data$debt_cost)
k_current_ratio <- kurtosis(financial_data$current_ratio)
k_exp_sales_ratio <- kurtosis(financial_data$exp_sales_ratio)
k_liab_equity_ratio <- kurtosis(financial_data$liab_equity_ratio)
k_ltcap_fixedasset_ratio <- kurtosis(financial_data$ltcap_fixedasset_ratio)
k_equity_asset_ratio <- kurtosis(financial_data$equity_asset_ratio)
k_asset_turnover <- kurtosis(financial_data$asset_turnover)
k_int_sales_ratio <- kurtosis(financial_data$int_sales_ratio)
k_receiv_sales_ratio <- kurtosis(financial_data$receiv_sales_ratio)
k_pe <- kurtosis(financial_data$pe, na.rm = TRUE)
k_pb <- kurtosis(financial_data$pb, na.rm = TRUE)
k_ps <- kurtosis(financial_data$ps, na.rm = TRUE)

#correlation between X variables with Y (roe)
cor_roe <- cor(financial_data$roe, financial_data$roe)
cor_gm <- cor(financial_data$roe, financial_data$gm)
cor_op_exp_ratio <- cor(financial_data$roe, financial_data$op_exp_ratio)
cor_cf_ratio <- cor(financial_data$roe, financial_data$cf_ratio)
cor_debt_cost <- cor(financial_data$roe, financial_data$debt_cost)
cor_current_ratio <- cor(financial_data$roe, financial_data$current_ratio)
cor_exp_sales_ratio <- cor(financial_data$roe, financial_data$exp_sales_ratio)
cor_liab_equity_ratio <- cor(financial_data$roe, financial_data$liab_equity_ratio)
cor_ltcap_fixedasset_ratio <- cor(financial_data$roe, financial_data$ltcap_fixedasset_ratio)
cor_asset_turnover <- cor(financial_data$roe, financial_data$asset_turnover)
cor_equity_asset_ratio <- cor(financial_data$roe, financial_data$equity_asset_ratio)
cor_int_sales_ratio <- cor(financial_data$roe, financial_data$int_sales_ratio)
cor_receiv_sales_ratio <- cor(financial_data$roe, financial_data$receiv_sales_ratio)
cor_pe <- cor(financial_data$roe, financial_data$pe, use = "complete.obs")
cor_pb <- cor(financial_data$roe, financial_data$pb, use = "complete.obs")
cor_ps <- cor(financial_data$roe, financial_data$ps, use = "complete.obs")

#create vectors
mean_vec = c(avg_roe, avg_gm, avg_op_exp_ratio, avg_cf_ratio, avg_debt_cost, avg_current_ratio,
         avg_exp_sales_ratio, avg_liab_equity_ratio, avg_ltcap_fixedasset_ratio, avg_equity_asset_ratio,
         avg_asset_turnover, avg_int_sales_ratio, avg_receiv_sales_ratio, avg_pe, avg_pb, avg_ps)

med_vec = c(median_roe, median_gm, median_op_exp_ratio, median_cf_ratio, median_debt_cost, median_current_ratio, 
            median_exp_sales_ratio, median_liab_equity_ratio, median_ltcap_fixedasset_ratio, median_equity_asset_ratio,
           median_asset_turnover, median_int_sales_ratio, median_receiv_sales_ratio, median_pe, median_pb, median_ps)

sd_vec = c(sd_roe, sd_gm, sd_op_exp_ratio, sd_cf_ratio, sd_debt_cost, sd_current_ratio, sd_exp_sales_ratio, 
           sd_liab_equity_ratio, sd_ltcap_fixedasset_ratio, sd_equity_asset_ratio, sd_asset_turnover, 
           sd_int_sales_ratio, sd_receiv_sales_ratio, sd_pe, sd_pb, sd_ps)

min_vec = c(min_roe, min_gm, min_op_exp_ratio, min_cf_ratio, min_debt_cost, min_current_ratio, min_exp_sales_ratio, 
            min_liab_equity_ratio, min_ltcap_fixedasset_ratio, min_equity_asset_ratio, min_asset_turnover, 
            min_int_sales_ratio, min_receiv_sales_ratio, min_pe, min_pb, min_ps)

max_vec = c(max_roe, max_gm, max_op_exp_ratio, max_cf_ratio, max_debt_cost, max_current_ratio, max_exp_sales_ratio, 
            max_liab_equity_ratio, max_ltcap_fixedasset_ratio, max_equity_asset_ratio, max_asset_turnover, 
            max_int_sales_ratio, max_receiv_sales_ratio, max_pe, max_pb, max_ps)


q_vec = rbind(q_roe, q_gm, q_op_exp_ratio, q_cf_ratio, q_debt_cost, q_current_ratio,
                 q_exp_sales_ratio, q_liab_equity_ratio, q_ltcap_fixedasset_ratio, q_equity_asset_ratio,
                 q_asset_turnover, q_int_sales_ratio, q_receiv_sales_ratio, q_pe, q_pb, q_ps)

sk_vec = c(sk_roe, sk_gm, sk_op_exp_ratio, sk_cf_ratio, sk_debt_cost, sk_current_ratio,
             sk_exp_sales_ratio, sk_liab_equity_ratio, sk_ltcap_fixedasset_ratio, sk_equity_asset_ratio,
             sk_asset_turnover, sk_int_sales_ratio, sk_receiv_sales_ratio, sk_pe, sk_pb, sk_ps)

k_vec = c(k_roe, k_gm, k_op_exp_ratio, k_cf_ratio, k_debt_cost, k_current_ratio,
             k_exp_sales_ratio, k_liab_equity_ratio, k_ltcap_fixedasset_ratio, k_equity_asset_ratio,
             k_asset_turnover, k_int_sales_ratio, k_receiv_sales_ratio, k_pe, k_pb, k_ps)

cor_vec = c(cor_roe, cor_gm, cor_op_exp_ratio, cor_cf_ratio, cor_debt_cost, cor_current_ratio,
            cor_exp_sales_ratio, cor_liab_equity_ratio, cor_ltcap_fixedasset_ratio, cor_equity_asset_ratio,
            cor_asset_turnover, cor_int_sales_ratio, cor_receiv_sales_ratio, cor_pe, cor_pb, cor_ps
)

#summary for the financial_data
data_summary <- round(data.frame(
  average = mean_vec,
  std.dev = sd_vec,
  median = med_vec, 
  quantile = q_vec,
  min = min_vec,
  max = max_vec,
  skewness = sk_vec,
  kurtosis = k_vec,
  correlation = cor_vec), 3)

view(data_summary)

#correlation matrix

data_cor <- round(cor(financial_data[,3:18], use = "complete.obs"),3)

#Compute a matrix of correlation p-values
p.mat <- cor_pmat(financial_data[,3:18])

#plot correlation matrix
ggcorrplot(data_cor,
           hc.order = TRUE, type = "lower",
           p.mat = p.mat,
            
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE
)


# Create the function to obtain mode in R
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Create a function to classify types of skewness with x = mean, y = mode
skew_type <- function(x,y) {
  if (x > y) {
    return("positive")
  } else if(x < y) {
    return("negative")
  } else {
    return("no")
  }
}
#calculate mean, median, mode and skewness by sectors
financial_data <- financial_data %>%
  group_by(sector) %>%
  mutate(avg_roe_bysector = round(mean(roe),2),
         median_roe_bysector = round(median(roe),2),
         mode_roe_bysector = round(getmode(roe),2),
         skewness_roe = skew_type(round(mean(roe),2),round(getmode(roe),2))
         )

#plot a bar graph for ROE by sectors
ggplot(financial_data, aes(sector, avg_roe_bysector)) + 
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(y = "average ROE (%)", x = element_blank()) +
  geom_text(
    aes(label = avg_roe_bysector),
    colour = "white", size = 5,
    vjust = 1.5, position = position_dodge(.9)
  ) +
    theme(axis.text.x=element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        title = element_text(size = 20))

#box plot for ROE by sectors to check outliers
ggplot(financial_data, aes(x=sector, y=roe)) +
  geom_boxplot(width = .5,outlier.size = 1.5, outlier.shape = 20) +
  theme(axis.text.x=element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        title = element_text(size = 18)) +
  labs(y = "Average ROE")




#ROE distribution by sectors
dist_roe_plot <- ggplot(financial_data, aes(roe)) +
  geom_histogram(binwidth = 3, fill = "white", colour = "black") +
  #separate by sectors
  facet_wrap(~sector, ncol = 2) +
  theme(strip.text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=15))
  

#add mean, median & mode to distribution plots

dist_roe_plot + 
  #add line for mean
   geom_vline(aes(xintercept = avg_roe_bysector, group = sector), 
             colour = 'red',
             lwd = 0.5) +
  #add text for mean
  geom_text(aes(x=sector,
                y=avg_roe_bysector,
                label=paste("mean = ",avg_roe_bysector)), 
            size = 7, x = 70, y = 200,
            colour = "red") +
  #add line and text for median
  geom_vline(aes(xintercept = median_roe_bysector, group = sector), 
             colour = "blue",
             lwd = 0.5) +
  geom_text(aes(x=sector,
                y=roe,
                label=paste("median = ",median_roe_bysector)), 
            size = 7, x = 70, y = 150,
            colour = "blue") +
  
  #add line and text for mode
  geom_vline(aes(xintercept = median_roe_bysector, group = sector), 
             colour = "purple",
             lwd = 0.5) +
  geom_text(aes(x=sector,
                y=roe,
                label=paste("mode = ",mode_roe_bysector)), 
            size = 7, x = 70, y = 100, 
            colour = "purple") +
  #add type of skewness to each facet
  geom_text(aes(x=sector,
                y=roe,
                label= paste(skewness_roe, " skewness")), 
            size = 7, x = -90, y = 200, 
            )
  
 
  
  

# Specify breaks as a Date vector
datebreaks <- seq(as.Date("2016-12-31"), as.Date("2022-06-30"), by = "1 year")

#time-series plot for roe (Y variable)
time_series_roe <- financial_data %>%
  mutate(date2 = as.Date(date)) %>%
  group_by(date2, sector) %>%
  summarise(average_roe_bydate = mean(roe))

ggplot(time_series_roe, aes(x= date2, y= average_roe_bydate)) +
  geom_line() +
  facet_grid(rows = vars(sector)) +
  labs(x = "date",
       y = "average return on equity") +
  scale_x_date(breaks= datebreaks) +
  theme(text = element_text(family = "serif", size = 14),
        title = element_text(color = "#8b0000"),
        axis.line = element_line(color = "black"))
  



#run an OLS regression for all variables
ols_all <- lm(roe ~ gm + op_exp_ratio + cf_ratio + debt_cost + current_ratio + 
               exp_sales_ratio + liab_equity_ratio + ltcap_fixedasset_ratio +
               equity_asset_ratio + asset_turnover + int_sales_ratio +
               receiv_sales_ratio + pe + pb + ps,
             data = financial_data)

summary(ols_all)


#OLS by food sector
food_sector <- financial_data%>%
  filter(sector == "Food")

ols_food <- lm(roe ~ gm + op_exp_ratio + cf_ratio + debt_cost + current_ratio + 
                exp_sales_ratio + liab_equity_ratio + ltcap_fixedasset_ratio +
                equity_asset_ratio + asset_turnover + int_sales_ratio +
                receiv_sales_ratio + pe + pb + ps,
              data = food_sector)

summary(ols_food)

#OLS by electrical and cable sector
elec_cable_sector <- financial_data%>%
  filter(sector == "Electrical and Cable")

ols_elec_cable <- lm(roe ~ gm + op_exp_ratio + cf_ratio + debt_cost + current_ratio + 
                 exp_sales_ratio + liab_equity_ratio + ltcap_fixedasset_ratio +
                 equity_asset_ratio + asset_turnover + int_sales_ratio +
                 receiv_sales_ratio + pe + pb + ps,
               data = elec_cable_sector)

summary(ols_elec_cable)

#OLS by iron and steel
iron_steel_sector <- financial_data%>%
  filter(sector == "Iron and Steel")

ols_iron_steel <- lm(roe ~ gm + op_exp_ratio + cf_ratio + debt_cost + current_ratio + 
                       exp_sales_ratio + liab_equity_ratio + ltcap_fixedasset_ratio +
                       equity_asset_ratio + asset_turnover + int_sales_ratio +
                       receiv_sales_ratio + pe + pb + ps,
                     data = iron_steel_sector)

summary(ols_iron_steel)

#OLS by plastic
plastic_sector <- financial_data%>%
  filter(sector == "Plastic")

ols_plastic <- lm(roe ~ gm + op_exp_ratio + cf_ratio + debt_cost + current_ratio + 
                    exp_sales_ratio + liab_equity_ratio + ltcap_fixedasset_ratio +
                    equity_asset_ratio + asset_turnover + int_sales_ratio +
                    receiv_sales_ratio + pe + pb + ps,
                  data = plastic_sector)

summary(ols_plastic)

#OLS by textile
textile_sector <- financial_data%>%
  filter(sector == "Textile")

ols_textile <- lm(roe ~ gm + op_exp_ratio + cf_ratio + debt_cost + current_ratio + 
                    exp_sales_ratio + liab_equity_ratio + ltcap_fixedasset_ratio +
                    equity_asset_ratio + asset_turnover + int_sales_ratio +
                    receiv_sales_ratio + pe + pb + ps,
                  data = textile_sector)

summary(ols_textile)

#correlation matrix for each sector
#Food
food_cor <- round(cor(food_sector[,3:18], use = "complete.obs"),3)

p.mat <- cor_pmat(food_sector[,3:18])

ggcorrplot(food_cor,
           hc.order = TRUE, type = "lower",
           p.mat = p.mat,
           
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE
) +
  ggtitle("Food")

#Plastic
plastic_cor <- round(cor(plastic_sector[,3:18], use = "complete.obs"),3)

p.mat <- cor_pmat(plastic_sector[,3:18])

ggcorrplot(plastic_cor,
           hc.order = TRUE, type = "lower",
           p.mat = p.mat,
           
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE
) +
  ggtitle("Plastic")

#Textile
textile_cor <- round(cor(textile_sector[,3:18], use = "complete.obs"),3)

p.mat <- cor_pmat(textile_sector[,3:18])

ggcorrplot(textile_cor,
           hc.order = TRUE, type = "lower",
           p.mat = p.mat,
           
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE
) +
  ggtitle("Textile")

#Iron and Steel
iron_steel_cor <- round(cor(iron_steel_sector[,3:18], use = "complete.obs"),3)

p.mat <- cor_pmat(iron_steel_sector[,3:18])

ggcorrplot(iron_steel_cor,
           hc.order = TRUE, type = "lower",
           p.mat = p.mat,
           
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE
) +
  ggtitle("Iron and Steel")

#Electrical and Cable
elec_cable_cor <- round(cor(elec_cable_sector[,3:18], use = "complete.obs"),3)

p.mat <- cor_pmat(elec_cable_sector[,3:18])

ggcorrplot(elec_cable_cor,
           hc.order = TRUE, type = "lower",
           p.mat = p.mat,
           
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE
) +
  ggtitle("Electrical and Cable")
