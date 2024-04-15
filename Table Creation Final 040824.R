## ---------------------------
##
## Script name: Table Creation 
##
## Purpose of script: combine dfs to create table illustrating
## RD Estimates of the Change in Payer Share for Hospital Admissions at Age 65 
##
## Author: Julie Norman
## Email: jnorman@pavir.org
##
## Last Updated: 04/08/2024
##
## ---------------------------
##
## Notes:
##
## Dataframes located in folder 'Source Data' 
##
## Sections
##  1. Setup
##  2. Percentage data extraction/combining
##  3. Create observations row and combine data
##  4. Reformat for Latex table
##  5. Latex table creation
## ---------------------------

## Setup ##

library(tidyverse)
library(xtable)

## ---------------------------

## Percentage data extraction/combining ##

## combine df into list
df_ls <- list(
  `All Hospitalizations` = read_csv("Source Data/reg_results_24month.csv"),
  Emergent = (read_csv("Source Data/reg_results_Emergent_24month.csv") %>% 
                filter(subgroup == 1)),
  `Non-Emergent` = (read_csv("Source Data/reg_results_Emergent_24month.csv") %>% 
                      filter(subgroup == 0)),
  Urban = read_csv("Source Data/U_reg_results_24.csv"),
  Rural = read_csv("Source Data/R_reg_results_24.csv"),
  Black = read_csv("Source Data/Black or African American_reg_results_24.csv"), 
  White = read_csv("Source Data/White_reg_results_24.csv"),
  `PG 1-4` = read_csv("Source Data/1-4_reg_results_24.csv"), 
  `PG 5` = read_csv("Source Data/5_reg_results_24.csv"),
  `PG 7-8` = read_csv("Source Data/7-8_reg_results_24.csv")
)


## create base df for loop
base_df <- read_csv("Source Data/reg_results_24month.csv") %>%  
  filter(is.na(payer2) == TRUE & poly == "quad") %>%
  bind_rows(filter(., term == "z") %>%
              mutate(term = "standard error")) %>%
  mutate(outcome = substr(outcome, start = 8, stop = nchar(outcome))) %>% 
  select(outcome, term)

## Extract and combine into one df
for (df_name in names(df_ls)) {
  
  # Extract column from the current data frame
  loop_df <- df_ls[[df_name]] %>% 
    filter(is.na(payer2) == TRUE & poly == "quad") %>% 
    select(outcome, term, estimate, std.error) %>% 
    bind_rows(filter(., term == "z") %>%
                mutate(term = "standard error")) %>% 
    mutate(!!df_name := 
             ifelse(term == "standard error", std.error, estimate),
           outcome = substr(outcome, start = 8, stop = nchar(outcome))) %>%
    select(outcome, term, !!df_name)
  
  
  # append to dataframe
  base_df <- left_join(base_df, loop_df, relationship = "many-to-many"
                       )
}

## order rows for table
order_df <- data.frame(
  outcome = c("Medicare", "VA",  "VA Paid", "Commercial", "Other Federal", "Medicaid", "Other" ),
  order = c( 1, 2, 3, 4, 5, 6, 7 )
)

## combine and reorder 
finalperc_df <- base_df %>% 
  left_join(order_df) %>%
  mutate_if(is.numeric, ~round(. * 100, digits = 2)) %>% 
  arrange(order, desc(term)) %>% 
  select(-order) %>% 
  distinct() 

## ---------------------------

## Create observations row and combine data ##

## base df for loop
obfinal_df <- data.frame(
  outcome = c("Observations"),
  term = c(" ")
)

## Loop for obs row
for (df_name in names(df_ls)) {
  
  # pull observations
  ob_df <- df_ls[[df_name]] %>% 
    filter(is.na(payer2) == TRUE & poly == "quad") %>% 
    select(df, outcome) %>% 
    distinct()
  
  max <- max(ob_df$df)
  min <- min(ob_df$df)
  
  # check for non unique values 
  if (max == min) {
    obfinal_df <- obfinal_df %>% 
      mutate(!!df_name := max + 5) 
    
  } else {
    obfinal_df <- obfinal_df %>% 
      mutate(!!df_name := NA) 
    # if obs entry is NA then the source data has inconsistent entries
  }  
}

final_df <- rbind(finalperc_df, obfinal_df[1, ]) 

## ---------------------------

## Reformat for Latex table ##

# final column names
data_cols <- final_df %>% 
  select(-outcome, -term) %>% 
  colnames()


latexWrk_df <- final_df %>% 
  mutate_all(as.character) %>% 
  mutate(dup = duplicated(outcome)) %>% 
  mutate(` ` = ifelse(dup == TRUE, " ", outcome)) %>% 
  relocate(` `) %>% 
  select(-outcome, -dup)

# Loop for paren and decimal entry format
for(col_name in data_cols) {
  
  # parenthesis for standard error
  latexWrk_df$paren3 <- paste0("(", latexWrk_df[[col_name]], "0)")
  latexWrk_df$paren4 <- paste0("(", latexWrk_df[[col_name]], ")")
  
  latexWrk_df$paren <- ifelse(nchar(latexWrk_df[[col_name]]) == 3, 
                              latexWrk_df$paren3,
                              latexWrk_df$paren4)
  
  latexWrk_df[[col_name]] <- ifelse(latexWrk_df$term == "standard error", latexWrk_df$paren,  latexWrk_df[[col_name]])

  # display two decimal places 
  latexWrk_df[[col_name]] <- ifelse(nchar(latexWrk_df[[col_name]]) == 1, 
                                    paste0(latexWrk_df[[col_name]], ".00"),
                                    gsub("\\b(\\d+\\.\\d{1})\\b", "\\10",
                                         latexWrk_df[[col_name]])
  )
}

latexfinal_df <- latexWrk_df %>% 
  select(-contains("paren"), -term)  

## Export    
  # write.csv(latexfinal_df)

## ---------------------------

## Latex Table Creation ##


latex_rawtbl <- xtable(latexfinal_df, floating = TRUE, include.rownames = FALSE,
                       colheads = unlist(strwrap(colnames(latexfinal_df), width = 10)),
                       booktabs = TRUE)

# center percentage columns
align(latex_rawtbl) <- c("l", "l", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c")

# add title
attr(latex_rawtbl, "caption") <- "RD Estimates of the Change in Payer Share for Hospital Admissions at Age 65"

# Convert xtable object to LaTeX code
latex_table <- print(latex_rawtbl, 
                     size = "small", #adjustable
                     scalebox = 1.15, #adjustable
                     caption.placement = "top",
                     include.rownames = FALSE,
                     booktabs = TRUE,
                     floating = TRUE, floating.environment = "sidewaystable",
                     sanitize.text.function=identity,
                     add.to.row = list(pos = list(21), 
                                       command = "\\midrule\n"),
)

# Print the LaTeX code (copy and paste into latex doc)
# requires: \usepackage{booktabs} \usepackage{rotating}
cat(latex_table)
