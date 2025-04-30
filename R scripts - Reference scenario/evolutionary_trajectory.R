# Load the required libraries 
library(tidyverse)
library(dplyr)
library(purrr)
library(readr)

# A C++ simulation generates one CSV file for each unique combination 
# of parameters (BC, Liquidity) tested during an experiment. The output files follow a naming convention like:
# Example filename: "param_7_sim_BC3_L10_183418.csv"
# Each CSV file contains simulation results for a specific parameter set.

# An *experiment* refers to a batch run of simulations over various parameter combinations, 
# with all corresponding CSV files saved in a designated output directory (see C++ code).

# The code below lists all CSV files produced by the C++ simulation in the target folder.

# Define the directory where the simulation outputs are stored.
file_path <- "C:\\Users\\Francesco\\Desktop\\money_c++\\Main\\Figure2"  # Replace with your actual directory path
csv_files <- list.files(path = file_path, 
                        pattern = "*.csv", 
                        full.names = TRUE
                        )

# Read and combine all CSV files
all_data <- csv_files %>%
  map_df(~read_csv(.x, show_col_types = FALSE))  # This reads each file and row-binds them together

# Data from an experiment (i.e., a batch of BC, liquidity combinations) in a single dataframe
df_core_money <- 
  all_data %>%
  as_tibble()  %>% 
  mutate(
    BCRatio = as.factor(BCRatio),
    Liquidity = as.factor(Liquidity),
    Total = Cooperators + 
      Defectors + 
      DirectReciprocators + 
      IndirectReciprocators + 
      MoneyUsers
  ) %>% 
  mutate(
    ShareCooperators = Cooperators / Total,
    ShareDefectors = Defectors / Total,
    ShareDR = DirectReciprocators / Total,
    ShareIR = IndirectReciprocators / Total,
    ShareMoney = MoneyUsers / Total
  ) %>% 
  select(-(c("Cooperators", 
             "Defectors", 
             "DirectReciprocators", 
             "IndirectReciprocators", 
             "MoneyUsers"))) %>% 
  pivot_longer(
    cols = starts_with("Share"), 
    names_to = "Strategy", 
    values_to = "SurvivorCount") %>%
  mutate(Strategy =
           case_when(
             Strategy == "ShareCooperators" ~ "Cooperators",
             Strategy == "ShareDefectors" ~ "Defectors",
             Strategy == "ShareDR" ~ "Direct-reciprocators",
             Strategy == "ShareIR" ~ "Indirect-reciprocators",
             Strategy == "ShareMoney" ~ "Money-users",
           )
  )

######### Evolutionary trajectories of strategies and cooperation rates #########

# The experiment folder may contain more parameter combinations than actually used.
# Therefore, after loading all files, we give the possibility to apply filters to retain only the relevant ones.

# For example, here we reproduce Figure 2 of the main manuscript 

Figure2 <- df_core_money %>% 
  filter(BCRatio %in% c(2, 3, 5, 10)) %>%
  filter(Liquidity %in% c(0.25, 1, 10, 50)) %>% 
  ggplot(aes(x = Step)) + # x-axis: Simulation step/tick
  ylim(0, 1) +
  stat_summary(
    aes(y = CooperationRate),
    fun.data = "median_hilow",
    geom = "point",
    size = 0.95,
    alpha = 0.9,
    data = . %>% filter(Step != 0) # Exclude initialization
  ) +
  stat_summary(
    aes(y = CooperationRate),
    fun.data = "median_hilow",
    geom = "errorbar",
    alpha = 0.8,
    data = . %>% filter(Step != 0) # Exclude initialization
  ) +
  stat_summary(
    aes(y = SurvivorCount, color = Strategy), 
    fun.data = "median_hilow", 
    geom = "line",
    linewidth = 0.8
  ) +
  stat_summary(
    aes(y = SurvivorCount, fill = Strategy), 
    fun.data = "median_hilow", 
    geom = "ribbon", 
    alpha = 0.2
  ) +
  scale_fill_manual(values = c( # Ribbons
    "Cooperators" = "#FF6666",
    "Defectors" = "#E6C700",
    "Direct-reciprocators" = "#9966CC",
    "Indirect-reciprocators" = "#33AAFF",
    "Money-users" = "#33CC99"
  )) +
  scale_color_manual(values = c( # Medians
    "Cooperators" = "#CC0000",
    "Defectors" = "#B39700",
    "Direct-reciprocators" = "#663399",
    "Indirect-reciprocators" = "#0077CC",
    "Money-users" = "#009966"
  )) +
  labs(
    y = "Population Mix and Cooperation Rates"
  ) +
  facet_grid(Liquidity ~ BCRatio, labeller = label_both) +
  theme_minimal() +
  theme(
    legend.position = "none",  # removes all legends
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 18),
    panel.spacing.y = unit(1, "lines"),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

Figure2
