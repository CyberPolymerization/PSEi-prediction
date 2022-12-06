      
required_pkgs <- c('tidyverse'      , 'ggtext'    , 'rugarch', 
                   'BatchGetSymbols', 'cowplot'   , 'purrr'  , 
                   'scales'         , 'texreg'    , 'knitr'  , 
                   'kableExtra'     , 'forecast'  , 'writexl',
                   'WDI'            ,'tidyr'      , 'FinTS'  )

installed_pkgs <- installed.packages()

# find missing packages
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed_pkgs[, 1])]

if (length(missing_pkgs) == 0 ) {
  message("No missing dependencies")
} else {
  install.packages(missing_pkgs)
  message("All packages installed")
}



#  Different Episodes in PH:
# April 1990 to September 1993 - coup de etat, gulf war, financial crisis of 1990, and power crisis of 1992 (mt. Pinatubo eruption)
# 1. Asian Financial Crisis - July 1997 to December 1998
# 2. Global Financial Crisis - 
# 3. COVID-19 pandemic - start dec. 31, 2019

first_date <- '1990-01-01' 
last_date <- '2022-11-11' 
my_ticker <- '^PSE'
series_name <- 'PSEi' 


n_largest <- 10 # number of largest absolute returns to plot

max_lag <- 5
my_html_file <- 'tabs/tab03-Arch_Test.html'
my_xlsx_file <- 'tabs/tab03-Arch_Test.xlsx'

ar_lag <- 0 
ma_lag <- 0 
arch_lag <- 1 
garch_lag <- 1 
max_lag_AR <- 1 
max_lag_MA <- 1 
max_lag_ARCH <- 2 
max_lag_GARCH <- 2
# models_to_estimate <- c('sGARCH', 'gjrGARCH') # see rugarch manual for more
# models_to_estimate <- c('sGARCH','gjrGARCH','eGARCH','fiGARCH','iGARCH') #12/06/22 -> Exclude FIGARCH
models_to_estimate <- c('sGARCH','gjrGARCH','eGARCH','iGARCH')
# 11/03/22: ORIGINAL has eGARCH
# models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH') # see rugarch manual for more 
distribution_to_estimate <- 'norm' # distribution used in all models
dist_to_use <- c('norm', 'std')
my_html_file <- 'tabs/tab04-estimation_garch.html' # where to save html file?

set.seed(20200315) 
n_sim <- 5000 
n_days_ahead <- 15*365 # Number of days ahead to simulate (10*365 in paper)






# END OPTIONS

# load required libraries
library(BatchGetSymbols)
library(tidyverse)
# library(stats) #for FFT
library(cowplot)
library(forecast)
# library(WDI) #get PH Inflation Data 11/12/22
library(knitr)
library(kableExtra)
library(writexl)
library(FinTS)
library(texreg)
library(rugarch)
library(purrr)
library(ggtext)



# change directory to where the script located
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# makes sure the directory "data" exists
if (!dir.exists('data')) dir.create('data')


# source functions 
source('fcts/garch_fcts.R')



l_out <- BatchGetSymbols(tickers = my_ticker, 
                         first.date = first_date, 
                         last.date = last_date)

# select columns and calculated log_ret and arim_ret
df_prices <- l_out$df.tickers %>%
  select(ref.date, ticker, price.adjusted) %>%
  mutate(log_ret = log(price.adjusted/dplyr::lag(price.adjusted)),
         arim_ret = price.adjusted/dplyr::lag(price.adjusted) - 1,
         series_name = series_name) %>%
  na.omit() # remove all NA values


rds_out <- 'data/RAC-GARCH-Data.rds'
write_rds(df_prices, rds_out)


# get price data
df_prices <- read_rds('data/RAC-GARCH-Data.rds')
series_name <- df_prices$series_name[1]


p1 <- ggplot(df_prices, aes(x = ref.date, y = price.adjusted)) + 
  geom_line() + 
  labs(title = paste0('Adjusted Prices of ', series_name),
       x = 'Date',
       y = 'Index Value (PHP)',
       caption = 'Data from Yahoo Finance') + 
  theme_bw(base_family = "TT Times New Roman") 

# calculate largest absolute price variations
largest_tab <- df_prices %>%
  group_by(ticker) %>%
  top_n(abs(log_ret), n = n_largest)

# create second plot
# was df_prices -> df_prices_decompose_random #arnel
p2 <- ggplot(df_prices , 
             aes(x = ref.date, y = log_ret)) + 
  geom_line() + 
  labs(title = paste0('Nominal Daily Log Returns of ', series_name),
       subtitle = paste0('Red circles represent the largest ', n_largest, 
                         ' absolute price variations in the sample'),
       x = 'Date',
       y = 'Log Returns',
       caption = 'Data from Yahoo Finance') + 
  theme_bw(base_family = "TT Times New Roman") +
  geom_point(data = largest_tab, aes(x = ref.date, y = log_ret), 
             size = 3, color = 'red'  ) +
  scale_y_continuous(labels = scales::percent) + 
  labs(size = 'Absolute Price Variation') # + 
scale_color_brewer(palette = 'BrBG')

# bind plots together
p <- plot_grid(p1, p2, nrow = 2, 
               labels = 'AUTO')

# show and save
# ERROR in old Code: invalid 'bg' value
#x11() ; p ; ggsave(filename = paste0('figs/fig02_', series_name, '_prices_returns.png'), 
#                  plot = p) 

x11() ; p1 ; ggsave(filename = paste0('figs/fig02a_', series_name, '_prices.png'), 
                    plot = p1, width = 12, height = 6)
x11() ; p2 ; ggsave(filename = paste0('figs/fig02b_', series_name, '_returns.png'), 
                    plot = p2, width = 12, height = 6)

# build autocorrelagram
p <- ggAcf(x = df_prices$log_ret, lag.max = 10) +
  labs(title = paste0('Autocorrelogram for the Log Returns of ', series_name)) +
  theme_bw(base_family = "TT Times New Roman")

x11()  ; p ; ggsave('figs/fig03_autocorrelation_logret.png', width = 12, height = 6)


# do arch test
tab_out <- do_arch_test(x = df_prices$log_ret, max_lag = max_lag)

tab_out

# remove attributes of table so it can be correctly parsed in html
tab_out <- as.data.frame(
  lapply(tab_out, function(x) { attributes(x) <- NULL; x })
)
str(tab_out)

rownames(tab_out) <- NULL

# save table in html
my_tbl <- knitr::kable(tab_out, format = 'html' ) %>%
  kable_styling(bootstrap_options = c("striped"), 
                full_width = FALSE ) 

my_tbl

cat(my_tbl, file = my_html_file)  

# write to excel
write_xlsx(x = tab_out, path = my_xlsx_file)
#write_xlsx(x = tab_out, path = my_xlsx_file) 


# get all combinations of models
df_grid <- expand_grid(ar_lag,
                       ma_lag,
                       arch_lag,
                       garch_lag,
                       models_to_estimate,
                       distribution_to_estimate)

estimate_garch <- function(ar_lag,
                           ma_lag,
                           arch_lag,
                           garch_lag,
                           models_to_estimate,
                           distribution_to_estimate) {
  
  message('Estimating ARMA(',ar_lag,',', ma_lag, ')', '-',
          models_to_estimate, '(', arch_lag, ',', garch_lag, ') ', 
          'dist = ', distribution_to_estimate)
  
  # estimate model
  my_spec <- ugarchspec(variance.model = list(model = models_to_estimate,
                                              garchOrder = c(arch_lag,garch_lag)),
                        mean.model = list(armaOrder = c(ar_lag,ma_lag)), 
                        distribution.model = distribution_to_estimate)
  
  # , 
  # include.mean = TRUE
  
  
  
  # Original ugarchspec - edited 10/30/22
  # my_spec <- ugarchspec(variance.model = list(model = models_to_estimate,
  #                                             garchOrder = c(arch_lag, 
  #                                                            garch_lag)),
  #                       mean.model = list(armaOrder = c(ar_lag,
  #                                                       ma_lag)),
  #                       distribution.model = distribution_to_estimate)
  
  
  # ugarchfit(spec = ugarchspec(mean.model = list(armaOrder = c(0, 0),
  #                                               include.mean = TRUE), variance.model = list(model = "eGARCH", garchOrder = c(1, 1))), 
  #           solver.control = list(tol = 1e-12),data = ABC)
  
  
  my_garch <- ugarchfit(spec = my_spec, data = df_prices$log_ret)
  # 10/30/22: added solver.control = list(tol = 1e-6)
  # 10/30/22: added solver = 'hybrid'
  
  
  return(my_garch)
}

# estimate all models
l_args <- as.list(df_grid)
l_models <- pmap(.l = l_args, .f = estimate_garch)

# make sure dir "tabs" exists
if (!dir.exists('tabs')) dir.create('tabs')

# reformat models for texreg
l_models <- map(l_models, extract.rugarch, include.rsquared = FALSE)

# write custom row
custom_row <- list('Variance Model' = df_grid$models_to_estimate,
                   'Distribution' = df_grid$distribution_to_estimate)
custom_names <- paste0('Model ', 1:length(l_models))

# save to html
htmlreg(l_models, 
        file = my_html_file, 
        custom.gof.rows = custom_row,
        custom.model.names = custom_names, 
        digits = 3)

# print to screen
screenreg(l_models,
          custom.gof.rows = custom_row,
          custom.model.names = custom_names, 
          digits = 3)


out <- find_best_arch_model(x = df_prices$log_ret, 
                            type_models = models_to_estimate,
                            dist_to_use = dist_to_use,
                            max_lag_AR = max_lag_AR,
                            max_lag_MA = max_lag_MA,
                            max_lag_ARCH = max_lag_ARCH,
                            max_lag_GARCH = max_lag_GARCH)

# get table with estimation results
tab_out <- out$tab_out

# pivot table to long format (better for plotting)
df_long <- tidyr::pivot_longer(data = tab_out %>%
                                 select(model_name,
                                        type_model,
                                        type_dist,
                                        AIC, BIC),  cols = c('AIC', 'BIC'))

models_names <- unique(df_long$model_name)
best_models <- c(tab_out$model_name[which.min(tab_out$AIC)],
                 tab_out$model_name[which.min(tab_out$BIC)])

# figure out where is the best model
df_long <- df_long %>%
  mutate(order_model = if_else(model_name %in% best_models, 'Best Model', 'Not Best Model') ) %>%
  na.omit()

# make table with best models
# Modifications: 11/13/22 - type_model must be execute first prior value, because first element of type model was being used not the
# best model
df_best_models <- df_long %>%
  group_by(name) %>%
  summarise(model_name = model_name[which.min(value)],
            type_model = type_model[which.min(value)],
            value = value[which.min(value)])


# ORIGINAL PLOT (11/13/22) - DOESNT LOOKS GOOD FOR 5 GARCH MODELS
# # plot results
# p1 <- ggplot(df_long %>%
#                arrange(type_model), 
#              aes(x = reorder(model_name, 
#                              order(type_model)),
#                  y = value, 
#                  shape = type_dist,
#                  color = type_model)) + 
#   geom_point(size = 3.5, alpha = 0.65) + 
#   coord_flip() + 
#   theme_bw(base_family = "TT Times New Roman") + 
#   facet_wrap(~name, scales = 'free_x') + 
#   geom_point(data = df_best_models, mapping = aes(x = reorder(model_name, 
#                                                               order(type_model)),
#                                                   y = value), 
#              color = 'blue', size = 5, shape = 8) +
#   labs(title = 'Selecting Garch Models by Fitness Criteria', 
#        subtitle = 'The best model is the one with lowest AIC or BIC (with star)',
#        x = '',
#        y = 'Value of Fitness Criteria',
#        shape = 'Type of Dist.',
#        color = 'Type of Model') + 
#   theme(legend.position = "right")
# 
# x11()  ; p1 ; ggsave('figs/fig04_best_garch.png', width = 24, height = 12)
# 



# plot results - edited 11/13/22 for cleaner plots
p1 <- ggplot(df_long %>%
               arrange(type_model), 
             aes(x = reorder(type_model, 
                             order(type_model)),
                 y = value, 
                 shape = type_dist,
                 color = type_model)) + 
  geom_point(size = 6, alpha = 0.4) + 
  coord_flip() + 
  theme_bw(base_family = "TT Times New Roman") + 
  facet_wrap(~name, scales = 'free_x') + 
  geom_point(data = df_best_models, mapping = aes(x = reorder(type_model, 
                                                              order(type_model)),
                                                  y = value), 
             color = 'blue', size = 8, shape = 8) +
  annotate("text", label = "Best Model:", x = 0.8, y = 0.025 + df_best_models$value[1], size = 3) +
  annotate("text", label = df_best_models$model_name[1], x = 0.65, y = 0.025 + df_best_models$value[1], size = 3) +
  labs(title = 'Selecting Garch Models by Fitness Criteria', 
       subtitle = 'The best model is the one with lowest AIC or BIC (with star)',
       x = '',
       y = 'Value of Fitness Criteria',
       shape = 'Type of Dist.',
       color = 'Type of Model') + 
  theme(legend.position = "right", plot.subtitle = element_text(face = "bold"))

x11()  ; p1 ; ggsave('figs/fig04_best_garch_WITHOUT_FIGARCH_11_13_22.png', width = 10, height = 5)


# estimate best garch model by BIC (used in next section)
best_spec = ugarchspec(variance.model = list(model =  out$best_bic$type_model, 
                                             garchOrder = c(out$best_bic$lag_arch,
                                                            out$best_bic$lag_garch)),
                       mean.model = list(armaOrder = c(out$best_bic$lag_ar, 
                                                       out$best_bic$lag_ma)),
                       distribution = 'std')

my_best_garch <- ugarchfit(spec = best_spec, 
                           data = df_prices$log_ret)

write_rds(my_best_garch, 'data/garch_model.rds')


series_name <- df_prices$series_name[1]
my_garch <- read_rds('data/garch_model.rds')

# do simulations
# do_sim <- function(n_sim = 1000, n_t = 1000, my_garch, df_prices) {
df_sim <- do_sim(n_sim = n_sim,
                 n_t = n_days_ahead,
                 my_garch,
                 df_prices = df_prices)

glimpse(df_sim )

# calculate probabilities of reaching peak value
tab_prob <- df_sim %>%
  group_by(ref_date) %>%
  summarise(prob = mean(sim_price > max(df_prices$price.adjusted)))

n_years_back <- 4 #was 4
df_prices_temp <- df_prices %>%
  dplyr::filter(ref.date > max(ref.date) - n_years_back*365)

my_garch_name <- toupper(as.character(my_garch@model$modeldesc$vmodel))

#TODO: plot geom_ribbon to plot confidence intervals
p1 <- ggplot() +
  geom_line(data = df_prices_temp,
            aes(x = ref.date, y = price.adjusted), color = 'black', size = 0.75)  +
  geom_line(data = df_sim,
            aes(x = ref_date,
                y = sim_price,
                group = i_sim),
            color = 'grey',
            size = 0.25,
            alpha = 0.015) +
  theme_bw(base_family = "TT Times New Roman") +
  geom_hline(yintercept = max(df_prices_temp$price.adjusted)) +
  labs(title = paste0('Price Projections of ', series_name),
       subtitle = paste0('Total of ', n_sim, ' simulations based on a ',
                         my_garch_name,
                         ' model selected by BIC'),
       caption = 'Data from Yahoo Finance',
       x = '',
       y = 'Index Value (PHP)') +
  ylim(c(0.75*min(df_prices_temp$price.adjusted),
         1.25*max(df_prices_temp$price.adjusted))) +
  xlim(c(max(df_prices_temp$ref.date) - n_years_back*365,
         max(df_prices_temp$ref.date) + 2*365) )


# plot graphics
x11(); p1 ; ggsave(paste0('figs/fig05_', series_name, '_price_simulation.png'), width = 12, height = 6)

my_idx_date <- first(which(tab_prob$prob > 0.5))
df_date <- tibble(idx = c(first(which(tab_prob$prob > 0.01)),
                          first(which(tab_prob$prob > 0.5)),
                          first(which(tab_prob$prob > 0.75)),
                          first(which(tab_prob$prob > 0.95))),
                  ref_date = tab_prob$ref_date[idx],
                  prob = tab_prob$prob[idx],
                  my_text = paste0(format(ref_date, '%m/%d/%Y'),
                                   '\nprob = ', scales::percent(prob) ) )

# df_date$ref_date[n] # where n is when prob. is 50%
df_textbox <- tibble(ref_date = df_date$ref_date[2],
                     prob = 0.25,
                     label = paste0('According to the estimated _', my_garch_name, '_ model, ', 
                                    'the probability of asset **', series_name, '** to reach ',
                                    'its historical peak value of ', 
                                    format(max(df_prices$price.adjusted), 
                                           big.mark = ',',
                                           decimal.mark = '.'),
                                    ' higher than **50%** will be at **', format(ref_date, '%m/%d/%Y'), '**.') )

p2 <- ggplot(tab_prob, aes(x = ref_date, y = prob) ) + 
  geom_line(size = 2) + 
  labs(title = paste0('Probabilities of ', series_name, ' Reaching its Historical Peak'),
       subtitle = paste0('Calculations based on ', n_sim, ' number of simulations of ',
                         my_garch_name, 
                         ' model'),
       x = '',
       y = 'Probability') + 
  scale_y_continuous(labels = scales::percent) + 
  geom_point(data = df_date,
             aes(x = ref_date, y = prob), size = 5, color = 'red') + 
  geom_text(data = df_date, aes(x = ref_date, y = prob, 
                                label = my_text), 
            nudge_x = nrow(tab_prob)*0.085,
            nudge_y = -0.05,
            color ='red', check_overlap = TRUE) + 
  geom_textbox(data = df_textbox, 
               mapping = aes(x = ref_date, 
                             y = prob, 
                             label = label),
               width = unit(0.5, "npc"),
               #fill = "cornsilk",
               hjust = 0) + 
  theme_bw(base_family = "TT Times New Roman")

x11(); p2 ; ggsave(paste0('figs/fig06_', series_name, '_prob_reaching_peak.png'), width = 12, height = 6)





