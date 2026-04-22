# Reportino 3
# last update: 2/12/2025

options(java.parameters = "-Xmx8000m")

#### Packages, functions and default operations ####

# libraries to open files
library(openxlsx)

# libraries for data manipulation
library(janitor)
library(stringr)
library(dplyr)
library(tidyr)
library(glue)
library(forcats)

# libraries for statistical analysis
library(irr) # Cohen's Kappa coefficient calculation and testing
library(BlandAltmanLeh) # Bland-Altman analysis and original plots
library(caret) # confusion matrix creation

# libraries for robust regression models (not published)
library(robust) # implement the robust models
library(sfsmisc) #implement the robust F-Test: Wald test for multiple coefficients
# library(MASS) #IS CONFLICTUAL against other packages

# libraries for plots
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggtext)
library(grid)
library(ggpubr)
library(ggrepel)

# libraries for tables
library(gtsummary) 
library(flextable)
library(purrr)


wd_main = "C:/Users/sergio.coluccia/Desktop/reportino3"

# work directory
setwd(wd_main)

# where input data are stored
wd_input_data = "input_data"

# where output data will be generated
wd_output_data = "output_data"

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# ! if you don't want to run all 1-3 sections of the script run only these four lines
# then directly go to section 4
test <- 0 # upload workdata

if(test == 1){
  r_data <- dir(path = wd_main)[str_detect(string = dir(path = wd_main), pattern = ".RData")]
  r_data_time <- format(as.Date(substr(r_data, start = nchar("Reportino3_") + 1, stop = str_locate(string = r_data, pattern = ".RData")[,1] - 1 ), format = "%d-%m"))
  r_data_recent <- which.min(difftime(time2 = r_data_time, time1 = as.Date(format(as.Date(Sys.Date()), "%d-%m"), format = , "%d-%m")))
  
  load(file = r_data[r_data_recent])
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

cartels <- dir(paste(wd_main, wd_input_data, "prediction_ingr_apr25/", sep = "/"))
cartels <- cartels[str_detect(string = cartels, pattern = "Nutvalues_ingr")]


doouts <- 1 # print outputs


# by dataset and algorithm, define the list of dataset, before and after automatic frame filtering of:
df_fin_reportino3_list <- list() # nutritional values
df_fin_reportino3_ingr_list <- list()  # ingredients
dato_5runs_performances_list <- list() # performance error of nutritional values
dato_5runs_perf_aggr_all_data  <- list() # aggregated performance error of nutritional values
mispred_reportino3_list <- list() # list of mispredicted dishes
conf_matrix_list <- list() # list of performances of ingredients and confusion matrices coordinates (linking data to "data_ingr_performance_list")
data_ingr_performance_list  <- list() # list of performances of ingredients, confusion matrices coordinates, status of misdetection
dish_ingr_performances_metric_all_data <- list() # list of performances of ingredients


nutrients <- c("cal_gt", "fat_gt", "carb_gt", "prot_gt", "mass_gt")
nutrients_names <- c( "Energy content (kcal)", "Fat content (g)", "Carbohydrates content (g)", "Protein content (g)", "Mass (g)")

nutrients_sorted <- c( "mass_gt", "cal_gt", "prot_gt", "fat_gt", "carb_gt" )
nutrients_names_sorted <- c( "Mass (g)", "Energy content (kcal)", "Protein content (g)", "Fat content (g)", "Carbohydrates content (g)")

datsets_4data <- c("ita_corr", "ita_orig", "usa_corr", "usa_orig")
data_names_4data <- apply(expand.grid(c("IT FCBD", "US FCBD"), c("—correction", "—no correction")), 1, paste, collapse = "")[c(1, 3, 2, 4)]

datsets_2data <- datsets_4data[c(1, 3)]
data_names_2data <- data_names_4data[c(1, 3)]

## FUNCTIONS for: accordances over quantiles; performances (error functions); table- plot- related ###

quartili <- function(x){
  q4 <- quantile(x, probs = seq(0, 1, 0.25))
  return(q4)
}


quintili <- function(x){
  q5 <- quantile(x, probs = seq(0, 1, 0.20))
  return(q5)
}


labels.producer_quant <- function(x,q){
  len = length(q)
  if(! len %in% c(5, 6)){
    print("errore")
    break
  }
  z <- array(0,c(1,length(x)))
  for (i in 1:length(x)){
    if (x[i]<=q[2]) z[i]<-1
    if ((q[2]<x[i]) & (x[i]<=q[3])) z[i]<-2
    if ((q[3]<x[i]) & (x[i]<=q[4])) z[i]<-3
    if(len == 5){
      if (x[i]>q[4]) z[i]<-4
    }else{
      if ((q[4]<x[i]) & (x[i]<=q[5])) z[i] <- 4
      if (x[i]>q[5]) z[i] <- 5
    }
  }
  
  return(z)
}


metriche_quart <- function(matrice_accordo_quar_perc)
{
  matrice_accordo_quar_rounded <- round(matrice_accordo_quar_perc * 100, 2)
  perc_accordo_perfetto_quar <- sum(diag(matrice_accordo_quar_perc))
  perc_opposite_quar <- matrice_accordo_quar_perc[1,4] + matrice_accordo_quar_perc[4, 1]
  perc_quar_adjacent <- matrice_accordo_quar_perc[1,2] + matrice_accordo_quar_perc[2, 1] + matrice_accordo_quar_perc[2, 3] + matrice_accordo_quar_perc[3, 2] + matrice_accordo_quar_perc[3, 4] + matrice_accordo_quar_perc[4, 3]
  results <- list(matrice_accordo_quar_rounded = matrice_accordo_quar_rounded, perc_accordo_perfetto_quar = perc_accordo_perfetto_quar, perc_opposite_quar = perc_opposite_quar, perc_quar_adjacent = perc_quar_adjacent)
  
  return(as.list(results))
}

metriche_quin <- function(matrice_accordo_quin_perc)
{
  matrice_accordo_quin_rounded <- round(matrice_accordo_quin_perc * 100, 2)
  perc_accordo_perfetto_quin <- sum(diag(matrice_accordo_quin_perc))
  perc_opposite_quin <- matrice_accordo_quin_perc[1, 5] + matrice_accordo_quin_perc[5, 1]
  perc_quin_adjacent <- matrice_accordo_quin_perc[1, 2] + matrice_accordo_quin_perc[2, 1] + matrice_accordo_quin_perc[2, 3] + matrice_accordo_quin_perc[3, 2] + matrice_accordo_quin_perc[3, 4] + matrice_accordo_quin_perc[4, 3] + matrice_accordo_quin_perc[4, 5] + matrice_accordo_quin_perc[5, 4]
  results <- list(matrice_accordo_quin_rounded = matrice_accordo_quin_rounded, perc_accordo_perfetto_quin = perc_accordo_perfetto_quin, perc_opposite_quin = perc_opposite_quin,perc_quin_adjacent = perc_quin_adjacent)
  
  return(as.list(results))
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


RMSE <- function(x) sqrt(mean(x^2))
MSE <- function(x) mean(x^2)
MAE <- function(x) mean(abs(x))
MAPE <- function(x) 100*mean(x)

# MAAPE? https://www.sciencedirect.com/science/article/pii/S0169207013000121
# MAAPE <- function(x) 100*mean(atan(x))
# MAAPE <- function(x) 100*mean(arctan(x))


fn_add_median <- function(var) {
  dato_5runs_perf_long %>%
    filter( !grepl( "no correction", dataset) ) %>%
    dplyr::group_by(model, metric, nutrient) %>%
    #dplyr::group_by(.data[[variable]]) %>%
    #dplyr::arrange(.data[[variable]]) %>%
    dplyr::summarise_at(.vars = "value",
                        .funs =  list("mean" = mean,
                                      "median" = median
                        )) %>%
    ungroup() %>%
    group_by(model, metric) %>%
    dplyr::summarise_at(.vars = var,
                        .funs =  list("median" = median)) %>%
    ungroup()
}

stylize_min_values <- function(data, col) {
  data %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = data[[col]] == min(data[[col]]),
        columns = vars(col)
      )
    )
}
  
italic_fun <- function(value, ingredient, cutoff_low, cutoff_up){
  case_when((value < cutoff_low) ~ glue("<i>{ingredient}</i>"),
            (value >= cutoff_low & value <= cutoff_up) ~ glue("{ingredient}"),
            (value > cutoff_up) ~ glue("<b>{ingredient}</b>"),
            is.na(value) ~ glue("{ingredient}*")
  )
}


#### 1 - DATA CREATION and PREPROCESSING ####

# nutrition5k dataset from Thames 2022
df_nutrition5k <- clean_names(read.csv(file = paste(wd_main, wd_input_data, "dishes_merged_csv.csv", sep = "/"), sep = ";"))
names(df_nutrition5k)
dim(df_nutrition5k)
names(df_nutrition5k)[2] <- "recipe_id"

temp <- df_nutrition5k[, - c(1, 3:14) ] 
cols_to_sort <- temp %>% select_at(vars(almonds:almond_milk)) %>% colnames()

df_nutrition5k <-
  temp %>%
  relocate(sort(cols_to_sort), .after = "recipes") %>%
  replace(is.na(.), 0) %>%
  mutate_at(cols_to_sort, as.numeric) %>%
  replace(is.na(.), 1)

#cartel = cartels[1]
#cartel = cartels[2]

for( cartel in cartels ){
  
  cat("Working on", cartel, "\n")
  
  name_cartel <- paste(str_extract(string = cartel, pattern = "before|after"), "frame_filtering", sep = "_")
  
  if(doouts == 1){
    
    wd = paste(wd_main, wd_output_data, "nutritional_value_prediction", sep = "/")
    dir.create(wd, recursive = T)
    setwd(wd)
    
    wd = paste(wd, name_cartel, sep = "/")
    dir.create(wd)
    setwd(wd)
    
    dir.create("./figures/")
    dir.create("./tables/")
    dir.create("./support_data/")
    
  }
  
  datapath <- paste(wd_main, wd_input_data, "prediction_ingr_apr25", cartel, sep = "/")
  
  n_data <- dir(path = datapath)  
  n_data <- n_data[str_detect(string = n_data, pattern = "\\.csv") & !str_detect(string = n_data, pattern = "TRAINSET")]
  
  dato_aggregato <- list()
  dato_5runs <- list()
  
  
  k <- 0
  
  for( i in n_data ){
    
    s <- gsub(x = i, pattern = "filt1_", replacement = "")
    k <- k + 1
    
    dato_5runs[[k]] <- clean_names( read.csv( file = paste(datapath, i, "sep" = "/") ) )
    
    dato_5runs[[k]] <- dato_5runs[[k]] %>% 
      group_by(recipe_id, model) %>% 
      arrange(recipe_id) %>% 
      mutate(run = row_number(),
             dataset = as.character(na.omit(str_extract(string = i, pattern = datsets_4data))),
             model = gsub(substr(s, start = 4, stop = str_length(s) - 29), pattern = "_IN1k", replace = "")
      ) %>% replace(is.na(.), 0)
    
    temp <- dato_5runs[[k]] %>% #temporary stored in the temp object to obtain "dato_aggregato"
      group_by(recipe_id, model, dataset) %>%
      summarise_at(.vars = names(dato_5runs[[k]][, -c(1:2, (ncol(dato_5runs[[k]]) - 1:0))]), mean) %>% # averaging on the runs of macro and ingredients
      replace(is.na(.), 0)
    
    dato_aggregato[[k]] <- temp
    
    dato_5runs[[k]] <- dato_5runs[[k]] %>% select(recipe_id:mass_gt, run, dataset)
    
    cat(i, "--->", dato_5runs[[k]]$dataset[1], "\n")
    cat(i, "--->", dato_5runs[[k]]$model[1], "\n")
    cat("dimension:", dim(dato_5runs[[k]]), "\n \n")
    
  } 
  
  
  n_data <- dir(path = datapath)
  i <- n_data[str_detect(string = n_data, "TRAINSET")]
  
  if(length(i) > 0 ){
    
    dato_training_set <- clean_names( read.csv( file = paste(datapath, i, "sep" = "/") ) ) %>% 
      select(!model & !ends_with("_gt") & !starts_with( c("cal_", "mass_", "carb_", "prot_", "fat_")) )
    
    ingredients_training_set <- names(dato_training_set)[-1]
    
  }
  
  
  dato_aggregato[[k]] <- temp
  
  dato_5runs[[k]] <- dato_5runs[[k]] %>% select(recipe_id:mass_gt, run, dataset)
  
  dato_aggregato <- bind_rows(dato_aggregato) %>% replace(is.na(.), 0)
  
  #View(dato_aggregato)
  
  
  if(length(table(dato_aggregato$dataset)) == 4){
    
    datsets <- datsets_4data    
    data_names <- data_names_4data      
    
  }else{
    datsets <- datsets_2data
    data_names <- data_names_2data 
  }
  
  datsets_sorted <- rev(datsets)
  data_names_sorted <- rev(data_names)
  
  
  # just a try
  print(dato_aggregato[dato_aggregato$recipe_id == dato_aggregato$recipe_id[1], ], n  = "all")
  #length = 16 = 4 models x 4 dataset
  
  n_dishes <- length(unique(dato_aggregato$recipe_id))
  #nrow(dato_aggregato)/676
  
  ingredients_test_set <- 
    dato_aggregato %>%
    ungroup() %>%
    select(contains("_pred") & !starts_with( c("cal_", "mass_", "carb_", "prot_", "fat_")) ) %>%
    rename_all( ~ str_remove(.x, "_pred")) %>%
    colnames()
    
  
  df_fin_reportino3 <- merge(x = dato_aggregato, y = df_nutrition5k[, c("recipe_id", "num_ingr_per_dish", "ingredients")], by = "recipe_id", all.x = T)
  
  dim(df_fin_reportino3)
  
  dim(df_fin_reportino3)[1]/(sum(table(df_fin_reportino3$model, df_fin_reportino3$dataset) > 0))
  
  names(df_fin_reportino3)
  
  # just a try
  print(df_fin_reportino3[df_fin_reportino3$recipe_id == df_fin_reportino3$recipe_id[1], ], n  = "all")
  
  head(df_fin_reportino3[(df_fin_reportino3$recipe_id == df_fin_reportino3$recipe_id[1])&(df_fin_reportino3$model == df_fin_reportino3$model[1]), ])
  head(dato_aggregato[(dato_aggregato$recipe_id == dato_aggregato$recipe_id[1])&(dato_aggregato$model == dato_aggregato$model[1]), ])
  
  # are all the test data in nutrition5k?
  cat( "number of dishes in nutrition5k data is:", length(unique(df_nutrition5k$recipe_id)) ) #nutrition5k data
  cat( "number of dishes in test data from", cartel, "is:", length(unique(dato_aggregato$recipe_id)) )  # test data
  
  missing_dish <- which(!(unique(dato_aggregato$recipe_id) %in% unique(df_nutrition5k$recipe_id)))
  cat("this is the 'plate_only' (", dato_aggregato$recipe_id[missing_dish], ") which was removed from previous analysis")
  
  #print(dato_aggregato[dato_aggregato$recipe_id == dato_aggregato$recipe_id[missing_dish],], n = "all")
  #df_nutrition5k[df_nutrition5k$recipe_id == dato_aggregato$recipe_id[missing_dish],] # should be empty dataframe
  #df_nutrition5k[df_nutrition5k$recipe_id == df_nutrition5k$recipe_id[missing_dish],]
  
  
  ### final data ###
  if(doouts == 1){
    write.csv2(x = df_fin_reportino3, file = "./support_data/df_fin_reportino3.csv", row.names = F)
    # df_fin_reportino3 <- read.csv2(file = "./support_data/df_fin_reportino3.csv")
  }
  
  cat( "our sample by model and dataset: n = ", dim(df_fin_reportino3)[1]/(sum(table(df_fin_reportino3$model, df_fin_reportino3$dataset) > 0)) ) #  in quanto ho 676 x 4 modelli con 4 dataset,  1 algoritmo con 2 dataset
  
  
  df_fin_reportino3_list[[ which( cartel == cartels ) ]] <- df_fin_reportino3
  names(df_fin_reportino3_list)[ which( cartel == cartels ) ] <- cartel
  
  
  ### macro ###
  nomi_quart <- paste0(names(df_fin_reportino3)[4:13], "_quart")
  nomi_quint <- paste0(names(df_fin_reportino3)[4:13], "_quint")
  nomi_quantili <- c(nomi_quart, nomi_quint)
  
  
  ingr_var_end <- dim(df_fin_reportino3)[2]
  
  s <- dim(df_fin_reportino3)[2] + 1
  
  for(k in nomi_quantili){
    df_fin_reportino3[, s] <- NA
    names(df_fin_reportino3)[s] <- k
    s <- s + 1
  }
  
  
  for(i in names(table(df_fin_reportino3$dataset))){
    
    cat("Dataset:", i, "\n")
    
    for(j in names(table(df_fin_reportino3$model))){
      
      cat("Model:", j, "\n")
      
      temp <- subset(x = df_fin_reportino3, subset = (df_fin_reportino3$dataset == i)&(df_fin_reportino3$model == j), select = c(4:13))
      
      dims <- dim(temp)
      
      if(dims[1] == 0){
        cat("######## data unavailable ######## \n")
      }else{
        #describe(temp)
        
        comp.quartili <- apply(temp, 2, quartili)
        comp.quintili <- apply(temp, 2, quintili)
        
        
        for (k in 1:dims[2]){
          
          cat("creating percentiles for accordances...", "in", names(temp)[k], "\n")
          
          segm.quart <- as.numeric(labels.producer_quant(temp[, k], comp.quartili[, k]))
          segm.quin <- as.numeric(labels.producer_quant(temp[, k], comp.quintili[, k]))
          df_fin_reportino3[row.names(temp), nomi_quantili[k]] <- segm.quart
          df_fin_reportino3[row.names(temp), nomi_quantili[k + 10]] <- segm.quin
          
        }
        
      }
    }
  }
  
  
  ### final data with quantiles ###
  if(doouts == 9999){
    write.csv2(x = df_fin_reportino3, file = "./support_data/df_fin_reportino3_quantiles.csv", row.names = F)
  }
  
  
  #### 1a - DESCRIPTIVE STATISTICS #####
  cat( "\n ###########  DESCRIPTIVE STATISTICS ############" )
  
  
  temp <- df_fin_reportino3[, c(1,3, 9:13)] %>%
    group_by(recipe_id, dataset) %>%
    summarise_at(.vars = names(df_fin_reportino3)[ 9:13 ], mean) %>%
    relocate(any_of(nutrients_sorted)) %>% 
    ungroup() %>%
    select(!recipe_id)
  
  
  for(i in 1:length(names(table(temp$dataset)))){
    temp$dataset[temp$dataset == names(table(temp$dataset))[i]] <- data_names[i]
  }
  temp$dataset <- factor(temp$dataset, levels = data_names_sorted)
  
  tabella_nut_observed <- 
    temp %>%
    tbl_summary(
      by = dataset,
      label = setNames(as.list(nutrients_names_sorted), nm = names(temp)[ 1:5 ]),
      percent = "column",
      missing = "ifany",
      type = list(all_continuous() ~ "continuous2"),
      statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})", "{mean} ({sd})", "{min}, {max}"),
                       all_categorical() ~ "{n} ({p}%)"
      ),
      digits = c(everything() ~1, all_categorical()~ c(0, 1)),
      missing_text = "(Missing)"
    ) %>%
    modify_header(label ~ "Target variables",
                  all_stat_cols() ~ "**{level}**") %>%
    bold_labels() %>%
    italicize_levels() %>%
    as_flex_table
  tabella_nut_observed
  
  
  if(doouts == 1){
    cat("\n printing tables... \n")
    save_as_docx(tabella_nut_observed, path = "./tables/table_s2_descriptive_observed.docx")
  }
  
  
  ### OBSERVED VALUES TO COMPARE WITH "Tailoring" ###
  
  chose_model <- df_fin_reportino3$model[1] #choose the model
  
  test <- df_fin_reportino3 %>%
    filter(dataset %in% c("usa_corr", "ita_corr"),
           model == chose_model      # choosed model
    ) %>%
    select(c(recipe_id, dataset) | contains(substr(nutrients_sorted, 1, 4)) & !contains(c("quart", "quint")) ) %>%
    pivot_wider(
      names_from = dataset,
      values_from = contains(c("pred", "gt"))
    ) 
  
  # OBSERVED VALUES FOR ITA_CORR (TO COMPARE WITH "Tailoring")
  tabella_ita_corr_obs <-
    test %>%
    select("recipe_id" | contains("_gt_ita_corr") ) %>% 
    rename_with(.cols = -1, ~ str_remove(.x, "_gt_ita_corr")) %>%
    #filter(recipe_id != "dish_1557861216") %>% 
    relocate(recipe_id, mass, cal, prot, fat, carb) %>%
    summarise(across(-1, 
                     .fns = list(Min = min,
                                 Q1 = ~quantile(., 0.25),
                                 Me = median,
                                 Q3 = ~quantile(., 0.75),
                                 Max = max,
                                 Mean = mean,
                                 SD = sd
                     ))) %>%
    round(., 2) %>%
    pivot_longer(everything(), names_sep = '_', names_to=c('.value', 'Target variables'))  %>% 
    rename_with(.cols = 2:6, ~ gsub(".", "_", nutrients_names_sorted, fixed = TRUE)) %>%
    regulartable() %>% autofit()
  
  tabella_ita_corr_obs
  
  # OBSERVED VALUES FOR USA_CORR (TO COMPARE WITH "Tailoring")
  tabella_usa_corr_obs <-
    test %>% 
    select("recipe_id" | contains("_gt_usa_corr") ) %>% 
    rename_with(.cols = -1, ~ str_remove(.x, "_gt_usa_corr")) %>%
    #filter(recipe_id != "dish_1557861216") %>% 
    relocate(recipe_id, mass, cal, prot, fat, carb) %>%
    summarise(across(-1, 
                     .fns = list(Min = min,
                                 Q1 = ~quantile(., 0.25),
                                 Me = median,
                                 Q3 = ~quantile(., 0.75),
                                 Max = max,
                                 Mean = mean,
                                 SD = sd
                     ))) %>%
    round(., 2) %>%
    pivot_longer(everything(), names_sep = '_', names_to = c('.value', 'Target variables'))  %>% 
    rename_with(.cols = 2:6, ~ gsub(".", "_", nutrients_names_sorted, fixed = TRUE)) %>%
    regulartable() %>% autofit()
  
  tabella_usa_corr_obs
  
  
  if(doouts == 1){
    save_as_docx(tabella_ita_corr_obs, path = "./support_data/table-1b_match_ita-corr-vs-reportino1.docx")
    save_as_docx(tabella_usa_corr_obs, path = "./support_data/table-1a_match_usa-corr-vs-reportino1.docx")
  }
  
  
  names(test)
  s <- ncol(test) + 1
  
  # calculating residuals
  for(j in 1:10){ 
    
    temp <-  paste(paste("diff", substr(names(test)[j + 1], start = 1, stop = 3), sep = "-"), as.character(na.omit(str_extract(string = names(test)[j + 1], pattern = datsets))), sep = "_" )
    test[, ncol(test) + 1] <- NA
    
    names(test)[ncol(test)] <- temp
    cat("\n difference between:",  names(test)[j + 1], "and", names(test)[j + 10 + 1] , "\n")
    
    test[, temp] <- test[, j + 1] - test[, j + 10 + 1]
    
  }
  
  
  # TABLE RESIDUALs (model chosed above)
  tabella <- test %>% summarise(across(names(test)[s:(s + 9)], 
                                       .fns = list(Min = min,
                                                   Q1 = ~quantile(., 0.25),
                                                   Me = median,
                                                   Q3 = ~quantile(., 0.75),
                                                   Max = max,
                                                   Mean = mean,
                                                   SD = sd
                                       )
  )
  ) %>% 
    select(contains("usa_corr")) %>%
    rename_all( ~ str_remove(.x, "_usa_corr")) %>%
    round(., 2) %>%
    pivot_longer(everything(), names_sep='_', names_to = c('.value', paste( 'Residuals of target variables \n of', chose_model ) )) %>%
    rename_at(.vars = -1, ~ nutrients_names_sorted) %>%
    regulartable() %>% autofit()
  
  tabella
  
  
  #### 1b - PERFORMANCEs OF ALOGIRTHMs ####
  cat( "\n ########## PERFORMANCEs OF ALOGIRTHMs ########### \n")
  
  
  temp <- bind_rows(dato_5runs)
  dim(temp)[1]/(length(table(temp$run))*(sum(table(df_fin_reportino3$model, df_fin_reportino3$dataset) > 0))) # == 676, in quanto ho 676 x 5 run x 4 modelli con 4 dataset,  1 algoritmi con 2 dataset
  dato_5runs <- as.data.frame(temp)
  
  
  runs <- names(table(dato_5runs$run))
  s1 <- 3
  s2 <- 7
  nomi_diffs <- apply(expand.grid(names(dato_5runs)[s1:s2], c("res", "abs")), 1, paste, collapse = "_")
  
  s_start <- dim(dato_5runs)[2] + 1
  s <- 0
  
  # CALCULATING DIEFFERENCE between PREDICTED and OBSERVED NUTRITIONAL VALUEs for METRICS
  for(k in 1:length(nomi_diffs)){
    
    dato_5runs[, s_start + s] <- NA
    names(dato_5runs)[s_start + s] <- nomi_diffs[k]
    
    cat("\n", names(dato_5runs)[s1 + (k-1) %% 5], "vs", names(dato_5runs)[8 + (k-1) %% 5], "\n")
    
    if( s <= 4 ){
      dato_5runs[, s_start + s] <- (dato_5runs[, s1 - 1 + k %% 6] - dato_5runs[, s2 + k %% 6])
    }else{
      
      cat("\n calcultating adjusted MAPE...\n")
      
      for(i in as.numeric(runs)){
        
        cat("adjusting '", substr(x = names(dato_5runs)[s1 + (k-1) %% 5], start = 1, stop = 3), "' for corrected MAPE", "in run", i, "\n")
        
        for (j in names(table(dato_5runs$model))){
          
          for(h in names(table(dato_5runs$dataset))){
            temp <- row.names(subset(dato_5runs, (dato_5runs$run == i)&(dato_5runs$model == j)&(dato_5runs$dataset == h)))
            
            if(length(temp) == 0){
              cat("########   ", h, ": data unavailable    ########\n")
            }else{
              media <- mean(dato_5runs[temp, s2 + 1 + k %% 6])
              dato_5runs[temp, s_start + s] <- abs((dato_5runs[temp, s2 + 1 + k %% 6] - dato_5runs[temp, s1 + k %% 6])/media)
            }
            
          }
          
        }
        
      }
      
    }
    
    s <- s + 1
    
  }
  
  
  dato_5runs_performances_list[[ which( cartel == cartels ) ]] <- dato_5runs
  names(dato_5runs_performances_list)[ which( cartel == cartels ) ] <- cartel
  
  
  s_end <- dim(dato_5runs)[2]
  cols_res <- dato_5runs %>% select(contains("_res")) %>% names()
  cols_abs <- dato_5runs %>% select(contains("_abs")) %>% names()
  
  
  dato_5runs_performances <- dato_5runs %>%
    select( 1, 2, s_end - 11:0 ) %>% 
    mutate(run = as.factor(run),
           dataset = as.factor(dataset)) %>%
    group_by(dataset, model, run) %>%
    summarise(across(any_of(cols_res),
                     list(RMSE =~ RMSE(.),
                          MAE =~ MAE(.)),
                     .names = "{.fn}_{.col}"),
              across(any_of(cols_abs),
                     list(MAPE = MAPE), .names = "{.fn}_{.col}")) %>% 
    ungroup() %>%
    group_by(dataset, model) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
    ungroup()
  
  
  dim(dato_5runs_performances)
  head(dato_5runs_performances)
  
  
  dato_5runs_perf_long <-
    dato_5runs_performances %>%
    filter( grepl("2\\+2", model) ) %>%
    group_by(dataset, model) %>% 
    pivot_longer(cols = 3:17, names_to = c('metric', 'nutrient'),
                 names_sep = '_', values_to = "value"
    )  %>% 
    mutate_at(.vars = "metric", ~factor(., levels = c("RMSE", "MAE", "MAPE"))) %>% 
    mutate(
      dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted),
      nutrient = factor(nutrient, levels = gsub(x = nutrients_sorted, pattern = "_gt", replacement = ""), labels = nutrients_names_sorted),
      pair = factor(ifelse(str_detect(string = dataset, pattern = "IT"), "IT", "US"), levels = c("US", "IT"))) %>%
    ungroup()
  
  
  condition_star <- sum(na.omit(str_detect(pattern = "—no correction", string = dato_5runs_perf_long$dataset))) > 0
  
  
  if( condition_star ){
    
    dato_5runs_perf_long <-
      dato_5runs_perf_long %>%
      group_by(metric, nutrient, dataset, model != "IncV3_2+2") %>%
      mutate(value2 = median(value) ) %>% 
      ungroup() %>%
      group_by(metric, nutrient, dataset) %>%
      mutate(point_level = median(value2) ) %>% 
      select(!c('model != "IncV3_2+2"', value2)) %>%
      ungroup()
    
  }else{
    
    dato_5runs_perf_long <-
      dato_5runs_perf_long %>%
      group_by(metric, nutrient, dataset) %>%
      mutate(point_level = median(value) ) %>% 
      ungroup()
    
  }
  
  # head(dato_5runs_performances)
  # head(dato_5runs_perf_long)
  
  dato_5runs_perf_aggr_all_data[[ which( cartel == cartels ) ]] <- dato_5runs_perf_long
  names(dato_5runs_perf_aggr_all_data)[ which( cartel == cartels ) ] <- cartel
  
  
  if(doouts == 1){
    # xlsx single models performance by run by nutrient and dataset
    write.csv2(x = as.data.frame(dato_5runs_perf_long), file = "./support_data/algorithms_median_performances.csv")
  }
  
  
  # when original data are considered not considering for computation of medians 
  condition_sentence <- NULL
  
  if(condition_star){
    condition_sentence <- "*not included in the computation of medians over the uncorrected datasets"
  }
  
  
  i <- 0
  q <- list()
  
  
  for(k in 1:nlevels(dato_5runs_perf_long$metric)){
    
    title_metric <- levels(dato_5runs_perf_long$metric)[k]             
    
    for(s in 1:nlevels(dato_5runs_perf_long$nutrient)){
      
      i <- i + 1
      labelling = NULL
      str_dir = NULL
      
      title_target <- levels(dato_5runs_perf_long$nutrient)[s]
      
      temp <- as.data.frame(subset(dato_5runs_perf_long, (dato_5runs_perf_long$metric == title_metric) & (dato_5runs_perf_long$nutrient == title_target))) %>%
        mutate(model = gsub(x = model, pattern = "_IN1k", replacement = ""),
               dataset = factor(dataset, levels = data_names_sorted,
                                labels = gsub(x = data_names_sorted, pattern = "-", replacement = "- \n " )
               )
        ) 
      
      
      if( condition_star ){
        temp <- temp %>% mutate(model = case_when( model == "IncV3_2+2" ~  "IncV3_2+2*", .default = model) )
      }
      
      
      lim2 <- 1.0 * max(temp$value)
      
      if( i <= 5){ 
        title_target_set = element_text(size = 9)
        title_data_set = element_text(size = 8)
      }else{
        title_target_set = element_blank()
        title_data_set = element_blank()
      }
      
      if( (i-1) %% 5 == 0){ 
        title_metric_set = element_text(size = 10, hjust = 0.5, vjust = -2, face = "bold.italic")
      }else{
        title_metric_set = element_blank() 
      }
      
      if(k == 3){
        labelling = scales::percent_format(scale = 1) 
      }else{
        labelling = scales::number_format()
      }
      
      temp_segments <- temp %>%
        distinct(dataset, point_level) %>%
        mutate(
          x = as.numeric(rev(dataset)),
          xstart = x - 0.3,
          xend   = x + 0.3
        )
      
      q[[i]] <- 
        ggplot(temp, aes(x = dataset, y = value)) +
        geom_col(aes(fill = model), position = position_dodge(1)) +
        labs(title = title_metric, x = title_target) +
        scale_x_discrete(limits = rev, drop = F) +
        coord_flip() +
        geom_point(data = temp_segments,
                   aes(x = dataset, y = point_level)
        ) +
        geom_segment(data = temp_segments,
                     aes(x = xstart, xend = xend, y = point_level, yend = point_level),
                     color = "black", size = 1.
        ) +
        geom_line(aes(x = dataset, y = point_level, group = pair), color = "red3", lty = "dotted", size = 0.8) +
        theme_minimal(base_size = 20) +
        scale_y_continuous(n.breaks = 7, labels = labelling) +
        theme(axis.title.y = title_target_set,
              axis.text.y = title_data_set,
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 8, vjust = 10),
              plot.title = title_metric_set,
              plot.margin = rep(unit(0, "null"), 4),
              panel.margin = unit(0, "null"),
              legend.position = "none") +
        scale_fill_manual(values = c("orange", "steelblue", "green3"))
      
    }
    
  }
  
  legend <-
    get_legend(
      ggplot(temp, aes(x = dataset, y = value, fill = model)) +
        geom_col(position = position_dodge(0.55)) +
        guides(fill = guide_legend(title = "Algorithm", title.position = "left", nrow = 1, byrow = TRUE)) +
        scale_fill_manual(values = c("orange", "steelblue", "green3"))
    )
  
  p <- plot_grid(plotlist = q, nrow = 5, byrow = F, rel_widths = c(1.3, 1, 1))
  
  title_plot_y <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0.5, label = "Target variable and dataset", size = 7, hjust = 0.5, vjust = .5, angle = 90)
  
  p <- plot_grid(plotlist = list(title_plot_y, p), ncol = 2, rel_widths = c(0.05, 1))
  
  title_plot_x <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0., label = "Dataset", size = 7, hjust = 0.5, vjust = -0.5)
  
  
  p <- 
    plot_grid(plotlist = list(p, legend), nrow = 2, rel_heights = c(1, 0.03))  + theme(plot.margin = margin(0, 8, 0, 0))
  
  
  if( condition_star ){
    
    p <-
      grid.arrange(p,
                   bottom = textGrob(
                     condition_sentence, x = 1, hjust = 1,
                     gp = gpar(fontface = 3L, fontsize = 9)
                   )
      )
    
  }
  
  p
  
  if(doouts == 9999){
    
    #par(mar = c(0, 0, 0, 0))
    
    ggsave2(
      filename = paste0("./figures/figure_all_performances.pdf"),
      plot = p,
      width = 12, height = 18,
      scale = 2,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
  ## MAPE aggregated
  p <- list()
  
  for(k in 1:nlevels(dato_5runs_perf_long$nutrient)){
    
    target <- levels(dato_5runs_perf_long$nutrient)[k]             
    
    temp <- as.data.frame(subset(dato_5runs_perf_long, (dato_5runs_perf_long$nutrient == target)&(dato_5runs_perf_long$metric == "MAPE")))
    
    
    if( condition_star ){
      temp <- temp %>% mutate(model = case_when( model == "IncV3_2+2" ~  "IncV3_2+2*", .default = model) )
    }
    
    
    temp <- 
      temp %>% ungroup() %>%
      mutate(pair = ifelse(str_detect(string = temp$dataset, pattern = "IT"), "IT", "US")) %>%
      select(!c(point_level, pair)) %>%
      group_by(nutrient, model) %>%
      mutate(value = median(value)) %>%
      select(!dataset) %>%
      arrange(desc(value)) %>%
      unique()
    
    
    p[[k]] <- 
      ggplot(temp, aes(fill = model, x = fct_reorder(model, value, .desc = F), y = value)) +
      geom_col(position = position_dodge(0.55)) + coord_flip() +
      theme_minimal(base_size = 28) +
      scale_y_continuous(name = nutrients_names_sorted[k], n.breaks = 7, limits = c(0, 60), labels = scales::percent_format(scale = 1)) +
      geom_text(aes(y = value, label = model), color = "black", fontface = "bold.italic", size = 4, hjust = 1.1) +
      theme(axis.text.x = element_text(size = 16),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            strip.text = element_text(size = 20),
            axis.ticks = element_blank(),
            legend.position = "none"
      ) +
      scale_fill_manual(values = c("orange", "steelblue", "green3"))
    
  }
  
  
  p <- plot_grid(plotlist = p, nrow = 3,ncol = 2, byrow = T)
  
  title_plot_x <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "MAPE", size = 9,vjust = 0.5)
  
  title_plot_y <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0.5, label = "Algorithm", size = 9, hjust = 0.5, vjust = 0.5, angle = 90)
  
  p <- plot_grid(plotlist = list(p, title_plot_x), nrow = 2, rel_heights = c(1, 0.05))
  
  p <- plot_grid(plotlist = list(title_plot_y, p), ncol = 2, rel_widths = c(0.05, 1))
  
  
  if( condition_star ){
    
    p <-
      grid.arrange(p,
                   bottom = textGrob(
                     condition_sentence, x = 1, hjust = 1,
                     gp = gpar(fontface = 3L, fontsize = 9)
                   )
      )
    
  }
  
  
  p
  
  
  if(doouts == 9999){
    
    par( mar = c(0, 0, 0, 0) )
    
    ggsave2(
      filename = paste0("./figures/plot_aggregated_MAPE.pdf"),
      plot = p,
      width = 24, height = 10,
      scale = 2,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
  
  k <- 0
  s <- 0
  tabella <- list() # list of tables of performances of single datasets and nutrients
  tabella2 <- list() # merging columns of tabella by nutrient
  tabella3a <- list() # list of tables of MEAN performances of single datasets and nutrients 
  tabella3b <- list() # list of tables of MEDIAN performances of single datasets and nutrients 
  
  # test: fn_add_median(var = "median")
  
  for(i in nutrients_names_sorted){
    
    s <- s + 1
    
    temp2 <- subset(dato_5runs_perf_long,
                    (dato_5runs_perf_long$nutrient == i) & !(str_detect(string = dato_5runs_perf_long$dataset, pattern = "no correction")) ) %>%  # nutriente i
      select(-nutrient) %>%
      group_by(model, metric) %>%                            # across dataset 
      summarise_at(.vars = "value",
                   .funs = list("mean" = mean,
                                "median" = median
                   )) %>%
      ungroup()
    
    for(j in names(table(dato_5runs_perf_long$dataset))){
      k <- k + 1
      
      temp <- subset(dato_5runs_perf_long,
                     (dato_5runs_perf_long$dataset == j)&(dato_5runs_perf_long$nutrient == i)) %>% 
        ungroup() %>%
        select(metric, value, model)
      
      
      tabella[[k]] <- 
        imap(c("value" = "{mean}"),
             ~ as.data.frame(temp) %>%
               tbl_continuous(by = metric,
                              variable = value,
                              statistic = ~.x,
                              label = list(model = j),
                              digits = c(everything() ~ 2)
               ) %>%
               bold_labels() %>%
               italicize_levels() %>%
               modify_header(
                 all_stat_cols() ~ "**{level}**") #%>%
        ) %>%
        tbl_merge(tab_spanner = FALSE) %>%
        modify_footnote(~NA) %>% modify_header(label = "**FCDB**",
                                               "stat_1_1" = "**RMSE**",
                                               "stat_2_1" = "**MAE**",
                                               "stat_3_1" = "**MAPE**")
      
    }
    
    tabella2[[s]] <- tbl_stack(tabella[(k - (length(data_names)- 1)):k])
    
    tabella3a[[s]] <-
      imap(c("Mean" = "{mean}"),
           ~ as.data.frame(temp2) %>% select(!median) %>%
             tbl_continuous(by = metric,
                            variable = mean,
                            statistic = ~.x,
                            label = list(model = i),
                            digits = c(everything() ~ 2)
             ) %>%
             bold_labels() %>%
             italicize_levels() %>%
             modify_header(
               all_stat_cols() ~ "**{level}**") #%>%
      ) %>%
      tbl_merge(tab_spanner = FALSE) %>%
      modify_footnote(~NA) %>% modify_header(label = "**Target variables**",
                                             "stat_1_1" = "**RMSE**",
                                             "stat_2_1" = "**MAE**",
                                             "stat_3_1" = "**MAPE**")
    
    
    tabella3b[[s]] <-
      imap(c("Median" = "{median}"),
           ~ as.data.frame(temp2) %>% select(!mean) %>%
             tbl_continuous(by = metric,
                            variable = median,
                            statistic = ~.x,
                            label = list(model = i),
                            digits = c(everything() ~ 2)
             ) %>%
             bold_labels() %>%
             italicize_levels() %>%
             modify_header(
               all_stat_cols() ~ "**{level}**") #%>%
      ) %>%
      tbl_merge(tab_spanner = FALSE) %>%
      modify_footnote(~NA) %>% modify_header(label = "**Target variables**",
                                             "stat_1_1" = "**RMSE**",
                                             "stat_2_1" = "**MAE**",
                                             "stat_3_1" = "**MAPE**")
    
    
    tabella3a[[s + 1]] <-
      imap(c("Mean" = "{mean}"),
           ~ as.data.frame(fn_add_median(var = "mean")) %>%
             tbl_continuous(by = metric,
                            variable = median,
                            statistic = ~.x,
                            label = list(model = "Overall"),
                            digits = c(everything() ~ 2)
             ) %>%
             bold_labels() %>%
             italicize_levels() %>%
             modify_header(
               all_stat_cols() ~ "**{level}**") #%>%
      ) %>%
      tbl_merge(tab_spanner = FALSE) %>%
      modify_footnote(~NA) %>% modify_header(label = "**Target variables**",
                                             "stat_1_1" = "**RMSE**",
                                             "stat_2_1" = "**MAE**",
                                             "stat_3_1" = "**MAPE**")
    
    tabella3b[[s + 1]] <-
      imap(c("Median" = "{median}"),
           ~ as.data.frame(fn_add_median(var = "median")) %>%
             tbl_continuous(by = metric,
                            variable = median,
                            statistic = ~.x,
                            label = list(model = "Overall"),
                            digits = c(everything() ~ 2)
             ) %>%
             bold_labels() %>%
             italicize_levels() %>%
             modify_header(
               all_stat_cols() ~ "**{level}**") #%>%
      ) %>%
      tbl_merge(tab_spanner = FALSE) %>%
      modify_footnote(~NA) %>% modify_header(label = "**Target variables**",
                                             "stat_1_1" = "**RMSE**",
                                             "stat_2_1" = "**MAE**",
                                             "stat_3_1" = "**MAPE**")
    
    
  }
  
  
  # table of performances over datasets by nutrient and metrics
  tabella_all_perf <- tbl_merge(tbls = tabella2, tab_spanner = nutrients_names_sorted) %>% as_flex_table()
  tabella_all_perf
  
  # table of dataset-aggregated performances over datasets by nutrient and metrics
  tabella3a <- tbl_stack(tbls = tabella3a) # mean values
  tabella3b <- tbl_stack(tbls = tabella3b) # median values
  
  tabella_aggr_perf <- 
    tbl_merge(list(tabella3a, tabella3b), tab_spanner = c("Mean Performance", "Median Performance")) %>% 
    modify_table_styling(
      columns = label,
      rows = label == "IncV3_2+2",
      footnote = gsub(x = condition_sentence, pattern = "\\*", replacement = "")
    ) %>% as_flex_table()
  
  tabella_aggr_perf
  
  
  if(doouts == 1){
    
    cat("...printing tables of performances...")
    
    save_as_docx(tabella_all_perf, path = paste0("./tables/table_s6_all-performances.docx"))  
    save_as_docx(tabella_aggr_perf, path = paste0("./tables/table_median_performances.docx"))
    
  }    
  
  
  #### 2 - BLAND-ALTMAN PLOTs, WILCOXON, ACCORDANCEs, ALL DISHes PREDICTED/OBSERVED VALUEs, LIST OF MISPREDICTED DISHes ####
  
  ##### 2a -  Bland-Altman plots, wilcoxon, accordances, all dishes predicted/observed values #####
  
  cat("\n \n ######### BLAND-ALTMAN PLOTs, WILCOXON, ACCORDANCEs, ALL DISHes PREDICTED/OBSERVED VALUEs, LIST OF MISPREDICTED DISHes \n ##########")
  
  datsets <- datsets_2data
  data_names <- data_names_2data 
  datsets_sorted <- rev(datsets)
  data_names_sorted <- rev(data_names)
  
  
  df_fin_reportino3 <- 
    df_fin_reportino3 %>% 
    filter( endsWith( model,("2+2") ) & grepl(x = dataset, pattern = "corr", fixed = F) )
  
  
  flag <- 0 # switch to 1 to draw the warned_dish_list (top 30 ingredients dishes)
  
  top_ingr <- c( "olive_oil", "salt", "garlic", "vinegar", "pepper", "onions", "lemon_juice", "broccoli", "carrot", "arugula", "parsley", "cherry_tomatoes", "spinach_raw", "cucumbers", "shallots", "chicken", "mustard", "mixed_greens", "cauliflower", "scrambled_eggs", "white_rice", "thyme", "wheat_berry", "bell_peppers", "bacon", "olives", "cheese_pizza", "berries", "lime", "jalapenos" )
  
  warned_dish_list <- (unique(df_fin_reportino3[(df_fin_reportino3$ingredients %in% top_ingr), "recipe_id"]))
  
  
  xlab_ba <- paste("Mean of predicted and observed values")
  ylab_ba <- paste("Difference between predicted and observed values")
  
  
  tabella_wilkx <- matrix(NA,
                          ncol = length(names(table(df_fin_reportino3$model))) + 2,
                          nrow = length(nutrients_names)*length(data_names))
  row.names(tabella_wilkx) <- do.call(what = paste, expand.grid(data_names, nutrients_names)) 
  colnames(tabella_wilkx) <- c("Outcome", "FCBD", names(table(df_fin_reportino3$model)))
  tabella_wilkx[, 1] <- rep(nutrients_names, each = length(data_names))
  tabella_wilkx[, 2] <- rep(data_names)
  
  tabella_quart_k <- tabella_wilkx
  tabella_quart_perf <- tabella_wilkx
  tabella_quart_opp <- tabella_wilkx
  tabella_quart_adj <- tabella_wilkx
  
  tabella_quin_k <- tabella_wilkx
  tabella_quin_perf <- tabella_wilkx
  tabella_quin_opp <- tabella_wilkx
  tabella_quin_adj <- tabella_wilkx
  
  vec_plot <- c(3, 5, 6, 4, 2)
  s <- 0
  sign_digt <- 3
  tabella2 <- NULL
  tabella <- NULL
  p1 <- list()
  p2 <- list()
  flag2 = ""
  
  
  for(k in 1:5){
    
    if(doouts == 1 & k == 1){
      
      n_data <- dir(path = "./support_data/")  
      n_data <- n_data[str_detect(string = n_data, pattern = "list_bland_altman|list_quantile")]
      n_data
      
      if(length(n_data) > 0){
        cat("\n found old data that must be delected...and they will \n")
        unlink( x = "./tables/list_*")
      }
      
    }
    
    wb_ba <- createWorkbook()
    wb_ba2 <- createWorkbook()        
    
    wb_qt <- createWorkbook()
    wb_qt2 <- createWorkbook()
    
    t <- 0
    
    y <- names(df_fin_reportino3)[3 + k]
    
    tabella <- 
      df_fin_reportino3 %>%
      select( c(2,3, 3 + k) ) %>%
      group_by(dataset, model) %>%
      summarise_at(vars(y), list(Minimum =~ quantile(., probs = 0.),  
                                 Q1 =~ quantile(., probs = 0.25),
                                 median =~ quantile(., probs = 0.5),
                                 Q3 =~ quantile(., probs = 0.75),
                                 Maximum =~ quantile(., probs = 1.),
                                 Mean = mean,
                                 SD = sd
                                 )
      ) %>%
      mutate(across(where(is.numeric), round, sign_digt),
             dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted),
             nutrient = nutrients_names[k]
      )
    
    
    tabella2 <- rbind(tabella2, tabella)
    
    
    tabella <- tabella %>% regulartable() %>% autofit()
    
    cat("Nutrient:", nutrients_names[k], "\n")
    
    
    for(i in names(table(df_fin_reportino3$dataset))){
      
      r <- 0
      t <- t + 1
      q <- list()
      list_dish <- list()
      list_dish_complete <- list()
      dato_dish_quantile <- list()
      dish_list_quantile_complete <- list()
      
      cat("------------------------ \n")
      cat("Dataset:", data_names[t], "\n")
      
      
      for(j in names(table(df_fin_reportino3$model))){
        
        #paste0("Data: ",data_names[t] ,";   Model: '", j, "'") #, "'; dataset '", data_names[i], "'") 
        title <- j 
        r <- r + 1
        s <- s + 1
        
        cat("Model:", j, "\n")
        
        subdata <- row.names( subset(x = df_fin_reportino3, subset = (df_fin_reportino3$dataset == i) & (df_fin_reportino3$model == j), select = c(2:11)) )
        
        if(length(subdata) == 0){
          
          cat("\n######## data unavailable ########\n")
          
        }else{
          
          cat("quartiles for", names(df_fin_reportino3)[c(8 + k)], "and", names(df_fin_reportino3)[c(3 + k)], "\n")
          matrice_accordo_prot_quart_perc <- prop.table(table(df_fin_reportino3[subdata, ingr_var_end + 5 + k], # observed
                                                              df_fin_reportino3[subdata, ingr_var_end + 0 + k]   # predicted
          )
          )
          
          metriche_quartili <- metriche_quart(matrice_accordo_prot_quart_perc)
          tabella_quart_perf[2*k - (2 - t), 2 + r] <- round( metriche_quartili[[2]], sign_digt ) 
          tabella_quart_opp[2*k  - (2 - t), 2 + r] <- round( metriche_quartili[[3]], sign_digt )
          tabella_quart_adj[2*k  - (2 - t), 2 + r] <- round( metriche_quartili[[4]], sign_digt )
          kappa_quart <- kappa2(df_fin_reportino3[subdata, ingr_var_end  + c(5, 0) + k])
          
          subdata2 <- as.character(round(kappa_quart$value, sign_digt))
          
          if(kappa_quart$p.value < 0.05){
            
            tabella_quart_k[2*k  - (2 - t), 2 + r] <- paste0(subdata2, "*")
            
          }else{
            
            tabella_quart_k[2*k  - (2 - t), 2 + r] <- subdata2
            
          }
          
          cat("\n quintiles for", names(df_fin_reportino3)[c(8 + k)], "and", names(df_fin_reportino3)[c(3 + k)], "\n")
          matrice_accordo_prot_quin_perc <-  prop.table(table(df_fin_reportino3[subdata, ingr_var_end + 5 + 5 + 5 + k], # observed
                                                              df_fin_reportino3[subdata, ingr_var_end + 5 + 5 + 0 + k]   # predicted
          )
          )
          
          metriche_quintili <- metriche_quin(matrice_accordo_prot_quin_perc)
          tabella_quin_perf[2*k - (2 - t), 2 + r] <- round( metriche_quintili[[2]], sign_digt )
          tabella_quin_opp[2*k  - (2 - t), 2 + r] <- round( metriche_quintili[[3]], sign_digt )
          tabella_quin_adj[2*k  - (2 - t), 2 + r] <- round( metriche_quintili[[4]], sign_digt )
          kappa_quin <- kappa2(df_fin_reportino3[subdata, ingr_var_end  + 5 + c(10, 5) + k])
          
          subdata2 <- as.character(round(kappa_quin$value, sign_digt))
          
          if(kappa_quin$p.value < 0.05){
            
            tabella_quin_k[2*k  - (2 - t), 2 + r] <- paste0(subdata2,"*")
            
          }else{
            
            tabella_quin_k[2*k  - (2 - t), 2 + r] <- subdata2
            
          }
          
          diff_test <- wilcox.test(x = df_fin_reportino3[subdata, 3 + k], y = df_fin_reportino3[subdata, 8 + k], paired = TRUE, conf.int = T)
          tabella_wilkx[2*k  - (2 - t), 2 + r] <- as.character(round(diff_test$p.value, sign_digt))
          
          ba_stat <- bland.altman.stats(df_fin_reportino3[subdata, 3 + k], df_fin_reportino3[subdata, 8 + k])
          eps <- (abs(min(ba_stat$diffs)) + max(ba_stat$diffs))/10
          breaks = as.numeric(sort(c(seq(min(ba_stat$diffs), max(ba_stat$diffs), by = eps), ba_stat$lines)))
          breaks <- sort(c(breaks[-which((breaks > ba_stat$lines[1] - eps)&(breaks < ba_stat$lines[3] + eps))], ba_stat$lines))
          labels = as.character(round(breaks,2))
          
          
          point_labs <- as.numeric(c(which(ba_stat$diffs < ba_stat$lower.limit, arr.ind = T), which(ba_stat$diffs > ba_stat$upper.limit, arr.ind = T)))
          
          list_dish_complete[[r]] <- cbind(df_fin_reportino3[subdata, c("recipe_id", "model", "dataset", "num_ingr_per_dish")], #[subdata[point_labs], c(1, 2, 3, 14, 15)], 
                                           predicted =  df_fin_reportino3[subdata, 3 + k], #[subdata, 3 + k],
                                           observed =  df_fin_reportino3[subdata, 8 + k],  #[subdata, 8 + k],
                                           nutrient = gsub(x = y, pattern = "_pred", replacement = ""),
                                           lower = ba_stat$lower.limit,
                                           upper = ba_stat$upper.limit,
                                           statement = ifelse(subdata %in% subdata[point_labs], 1, 0)
                                           )
          
          list_dish[[r]] <- cbind(df_fin_reportino3[subdata[point_labs], c("recipe_id", "model", "dataset", "num_ingr_per_dish")], 
                                  predicted =  df_fin_reportino3[subdata[point_labs], 3 + k],
                                  observed =  df_fin_reportino3[subdata[point_labs], 8 + k],
                                  nutrient = gsub(x = y, pattern = "_pred", replacement = ""),
                                  lower = ba_stat$lower.limit,
                                  upper = ba_stat$upper.limit
                                  )
          
          if(flag == 1){
            
            list_dish[[r]]$distance_loa = NA
            list_dish[[r]]$filter_7dish = NA
            list_dish[[r]]$filter_7dish_outloas = NA
            
            temp <-  (list_dish[[r]]$predicted - list_dish[[r]]$observed)
            temp <- data.frame(v1 = (ba_stat$lower.limit - temp)^2, v2 = (ba_stat$upper.limit - temp)^2)
            temp <- apply(X = temp, MARGIN = 1, FUN = which.min)
            temp2 <- data.frame(v1 = abs(ba_stat$lower.limit - (list_dish[[r]]$predicted - list_dish[[r]]$observed)),
                                v2 = abs(ba_stat$upper.limit - (list_dish[[r]]$predicted - list_dish[[r]]$observed))
            )
            
            for(j in 1:nrow(temp2)){
              list_dish[[r]]$distance_loa[j] <- temp2[ j, temp[j] ]
              list_dish[[r]]$filter_7dish[j] <- ifelse(list_dish[[r]]$recipe_id[j] %in% warned_dish_list, 1, 0)
              list_dish[[r]]$filter_7dish_outloas[j] <- 
                ifelse( sum(( list_dish[[r]]$recipe_id[j] %in% warned_dish_list ) & ( df_fin_reportino3[subdata[point_labs], "recipe_id"] %in% warned_dish_list )), 1, 0 )
            }
            
          }
          
          
          residui <- abs(df_fin_reportino3[subdata, 3 + k] - df_fin_reportino3[subdata, 8 + k])
          quantiles <- as.numeric(quantile(x = residui, probs = 0.95))
          point_labs <- as.numeric(which(findInterval(x = residui, vec = quantiles) %in% 1))
          
          dish_list_quantile_complete[[r]] <- cbind(df_fin_reportino3[subdata, c("recipe_id", "model", "dataset", "num_ingr_per_dish")], #[subdata[point_labs], c(1, 2, 3, 14, 15)], 
                                                    predicted =  df_fin_reportino3[subdata, 3 + k],
                                                    observed =  df_fin_reportino3[subdata, 8 + k],
                                                    residual = df_fin_reportino3[subdata, 3 + k] - df_fin_reportino3[subdata, 8 + k],
                                                    nutrient = gsub(x = y, pattern = "_pred", replacement = ""),
                                                    lower = NA,
                                                    upper = round(quantiles, 3),
                                                    statement = ifelse(subdata %in% subdata[point_labs], 1, 0)
                                                    
          ) 
          
          dato_dish_quantile[[r]] <- cbind(df_fin_reportino3[subdata[point_labs], c("recipe_id", "model", "dataset", "num_ingr_per_dish")], 
                                           predicted =  df_fin_reportino3[subdata[point_labs], 3 + k],
                                           observed =  df_fin_reportino3[subdata[point_labs], 8 + k],
                                           residual = df_fin_reportino3[subdata[point_labs], 3 + k] - df_fin_reportino3[subdata[point_labs], 8 + k],
                                           nutrient = gsub(x = y, pattern = "_pred", replacement = ""),
                                           q95 = round(quantiles, 3)
                                           ) 
          
          p <- bland.altman.plot(df_fin_reportino3[subdata, 3 + k], df_fin_reportino3[subdata, 8 + k], graph.sys = "ggplot2")
          
          #point_labs <- which(df_fin_reportino3[subdata, "recipe_id"] %in% warned_dish_list[[substr(y,start = 1, stop = 3)]])
          point_labs <- which(df_fin_reportino3[subdata, "recipe_id"] %in% warned_dish_list)
          
          q[[r]]  <- 
            p +
            labs(y = NULL, x = NULL, title = gsub(x = title, pattern = "_IN1k", replacement = "")) +
            geom_hline(yintercept = 0, lty = "dotted", col = "red", size = 1.2) +
            geom_hline(yintercept = ba_stat$lines, col = c("red4", "green4", "red4"), size = 1) +
            scale_y_continuous(limits = c(min(ba_stat$lower.limit, min(ba_stat$diffs)), max(max(ba_stat$diffs), ba_stat$upper.limit)),
                               breaks = breaks, labels = labels) + 
            scale_x_continuous(limits = c(min(ba_stat$means), max(ba_stat$means)),
                               n.breaks = length(breaks)) +
            theme_minimal(base_size = 12) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold.italic")) 
          
          if(flag == 1){
            
            q[[r]]  <-  q[[r]] +
              geom_point(data = data.frame(diffs = ba_stat$diffs[point_labs], means = ba_stat$means[point_labs]), 
                         mapping = aes(y = diffs, x = means),size = 2, shape = 15, col = "red") 
            
          }
          
        }
        
      }
      
      if(doouts == 1){
        
        flag2 <- ifelse(flag == 1, "30_dishes_", "")
        
        n_data <- dir(path = "./support_data//")  
        target <- paste0(data_names[t], "_", substr(y, start = 1, stop = 4))
        
        
        list_dish <- as.data.frame(bind_rows(list_dish)  %>%  group_by(recipe_id) %>% mutate(total_count = n()) %>% arrange())
        list_dish_complete <- bind_rows(list_dish_complete) %>% group_by(recipe_id) %>% mutate(total_count = sum(statement == 1, na.rm = TRUE)) %>% arrange() %>% as.data.frame
        dato_dish_quantile <- as.data.frame(bind_rows(dato_dish_quantile)  %>%  group_by(recipe_id) %>% mutate(total_count = n()) %>% arrange())
        dish_list_quantile_complete <- bind_rows(dish_list_quantile_complete)  %>%  group_by(recipe_id) %>% mutate(total_count = sum(statement == 1, na.rm = TRUE)) %>% arrange() %>% as.data.frame

        
        title_xlsx = paste0("list_bland_altman_", flag2, substr(y, start = 1, stop = 4) , ".xlsx")
        
        cat("printing file:", title_xlsx, ", sheet:", i, "...\n")
        addWorksheet(wb = wb_ba, sheetName = i)
        writeData(wb = wb_ba, sheet = i, x = list_dish, startRow = 1, startCol = 1)
        saveWorkbook(wb = wb_ba, file = paste0("./support_data//",title_xlsx), overwrite = TRUE)
        
        title_xlsx = paste0("list_complete_bland_altman_", flag2, substr(y, start = 1, stop = 4) , ".xlsx")
        
        cat("printing file:", title_xlsx, ", sheet:", i, "...\n")
        addWorksheet(wb = wb_ba2, sheetName = i)
        writeData(wb = wb_ba2, sheet = i, x = list_dish_complete, startRow = 1, startCol = 1)
        saveWorkbook(wb_ba2, file = paste0("./support_data/",title_xlsx), overwrite = TRUE)

        
        title_xlsx2 = paste0("list_quantile_", substr(y, start = 1, stop = 4) , ".xlsx")
        
        cat("printing file:", title_xlsx2, ", sheet:", i, "...\n")
        addWorksheet(wb = wb_qt, sheetName = i)
        writeData(wb = wb_qt, sheet = i, x = dato_dish_quantile, startRow = 1, startCol = 1)
        saveWorkbook(wb = wb_qt, file = paste0("./support_data/",title_xlsx2), overwrite = TRUE)
        
        title_xlsx2 = paste0("list_complete_quantile_", substr(y, start = 1, stop = 4) , ".xlsx")
        
        cat("printing file:", title_xlsx2, ", sheet:", i, "...\n")
        addWorksheet(wb = wb_qt2, sheetName = i)
        writeData(wb = wb_qt2, sheet = i, x = dish_list_quantile_complete, startRow = 1, startCol = 1)
        saveWorkbook(wb = wb_qt2, file = paste0("./support_data/",title_xlsx2), overwrite = TRUE)
        
      }
      
      p1[[t]] <-
        plot_grid(plotlist = q, nrow = 3,ncol = 1, byrow = T)
      
      title_plot <-   
        ggplot() +
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = data_names[t], size = 6, hjust = 0.5, vjust = 0.5)
      
      p1[[t]] <-
        plot_grid(plotlist = list(title_plot, p1[[t]]), nrow = 2, rel_heights = c(0.05, 1))
      
    }
    
    
    cat("\n ******")
    
    
    p2 <- plot_grid(plotlist = rev(p1), ncol = 2, nrow = 1)
    
    title_plot <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = nutrients_names[k], size = 12, hjust = 0.5, vjust = 0.5)
    
    title_plot_x <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = xlab_ba, size = 9,vjust = 0.5)
    
    title_plot_y <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0., y = 0.5, label = ylab_ba, size = 9, hjust = 0.5, vjust = 0.5, angle = 90)
    
    target <- substr(names(df_fin_reportino3)[3 + k], start = 1, stop = 3)
    
    p2 <- plot_grid(plotlist = list(p2, title_plot_x), nrow = 2, rel_heights = c(1, 0.05))
    
    p2 <- plot_grid(plotlist = list(title_plot_y, p2), ncol = 2, rel_widths = c(0.05, 1))
    
    p2 <- plot_grid(plotlist = list(title_plot, p2), nrow = 2, rel_heights = c(0.05, 1))
    
    p2
    
    
    if(doouts == 1){  
      
      cat("\n printing plots...")
      
      par( mar = c(0, 0, 0, 0) )
      
      ggsave2(
        filename = paste0("./figures/supfig", vec_plot[k], "_BA_", flag2,  target,".pdf"),
        plot = p2,
        width = 10, height = 14,
        scale = 3,
        units = c("cm"),
        dpi = 300,
        limitsize = TRUE)
      dev.off()
      
    }
    
  }
  
  if(doouts == 1){
    
    cat("\n printing tables of accordances and predicted values...\n")
    
    folder = paste0("./support_data/table_wilcoxon")
    tabella_wilkx <- as.data.frame(tabella_wilkx, row.names = subdata) 
    write.xlsx(x = tabella_wilkx, file = paste0(folder, ".xlsx"))
    tabella_wilkx <- tabella_wilkx %>% regulartable() %>% autofit()
    save_as_docx(tabella_wilkx, path = paste0(folder, ".docx"))
    
    
    folder = paste0("./tables/table_s3_k-quart")
    tabella_quart_k <- as.data.frame(tabella_quart_k) 
    write.xlsx(x = tabella_quart_k, file = paste0(folder, ".xlsx"))
    tabella_quart_k <- tabella_quart_k %>% regulartable() %>% autofit()
    save_as_docx(tabella_quart_k, path = paste0(folder, ".docx"))
    
    folder = paste0("./support_data/table_PERF-quart")
    tabella_quart_perf <- as.data.frame(tabella_quart_perf) 
    write.xlsx(x = tabella_quart_perf, file = paste0(folder, ".xlsx"))
    tabella_quart_perf <- tabella_quart_perf %>% regulartable() %>% autofit()
    save_as_docx(tabella_quart_perf, path = paste0(folder, ".docx"))
    
    folder = paste0("./support_data/table_ADJ-quart")
    tabella_quart_adj <- as.data.frame(tabella_quart_adj) 
    write.xlsx(x = tabella_quart_adj, file = paste0(folder, ".xlsx"))
    tabella_quart_adj <- tabella_quart_adj %>% regulartable() %>% autofit()
    save_as_docx(tabella_quart_adj, path = paste0(folder, ".docx"))
    
    folder = paste0("./support_data/table_OPP-quart")
    tabella_quart_opp <- as.data.frame(tabella_quart_opp) 
    write.xlsx(x = tabella_quart_opp, file = paste0(folder, ".xlsx"))
    tabella_quart_opp <- tabella_quart_opp %>% regulartable() %>% autofit()
    save_as_docx(tabella_quart_opp, path = paste0(folder, ".docx"))
    
    
    folder = paste0("./support_data/table_k-quin")
    tabella_quin_k <- as.data.frame(tabella_quin_k) 
    write.xlsx(x = tabella_quin_k, file = paste0(folder, ".xlsx"))
    tabella_quin_k <- tabella_quin_k %>% regulartable() %>% autofit()
    save_as_docx(tabella_quin_k, path = paste0(folder, ".docx"))
    
    
    folder = paste0("./support_data/table_PERF-quin")
    tabella_quin_perf <- as.data.frame(tabella_quin_perf) 
    write.xlsx(x = tabella_quin_perf, file = paste0(folder, ".xlsx"))
    tabella_quin_perf <- tabella_quin_perf %>% regulartable() %>% autofit()
    save_as_docx(tabella_quin_perf, path = paste0(folder, ".docx"))
    
    folder = paste0("./support_data/table_ADJ-quin")
    tabella_quin_adj <- as.data.frame(tabella_quin_adj) 
    write.xlsx(x = tabella_quin_adj, file = paste0(folder, ".xlsx"))
    tabella_quin_adj <- tabella_quin_adj %>% regulartable() %>% autofit()
    save_as_docx(tabella_quin_adj, path = paste0(folder, ".docx"))
    
    folder = paste0("./support_data/table_OPP-quin")
    tabella_quin_opp <- as.data.frame(tabella_quin_opp) 
    write.xlsx(x = tabella_quin_opp, file = paste0(folder, ".xlsx"))
    tabella_quin_opp <- tabella_quin_opp %>% regulartable() %>% autofit()
    save_as_docx(tabella_quin_opp, path = paste0(folder, ".docx"))
    
    folder = paste0("./tables/table_s2_descriptive_predicted")
    tabella2 <- as.data.frame(tabella2) 
    write.xlsx(x = tabella2, file = paste0(folder, ".xlsx"))
    tabella2 <- tabella2 %>% regulartable() %>% autofit()
    save_as_docx(tabella2, path = paste0(folder, ".docx"))
    
  }
  
  
  ##### 2b - mispredicted dishes #####
  n_data <- dir(path = "./support_data/")  
  n_data <- n_data[str_detect(string = n_data, pattern = "list_complete") &
                     !str_detect(string = n_data, pattern = "~$*")
                   ]
  n_data
  
  if( length(n_data) == 0 ){
  
    cat("\n...no data found for mispredicted dishes. This part will be jumped \n")
    
  }else{
    
    
    ###### 2ba - List of Dishes out-of-LoAs ######
    
    dato_dish_ba <- list() # list by dataset
    dato_dish_ba_aggr <- list() # merged list
    
    n_data <- dir(path = "./support_data/")  
    n_data <- n_data[str_detect(
                              string = n_data, pattern = "list_complete_bland_altman") &
                              !str_detect(string = n_data, pattern = "~$*")
                   ]
    n_data
    
    k <- 0
    s <- 0
    s2 <- 1
    
    for( i in n_data ){
      s <- s + 1
      
      for( j in datsets ){
        k <- k + 1
        
        dato_dish_ba[[k]] <- clean_names(read.xlsx(xlsxFile = paste0("./support_data/",i), sheet =  j))
        
        dato_dish_ba[[k]][,c("predicted", "observed")] <- apply(dato_dish_ba[[k]][,c("predicted", "observed")], 2, as.numeric)
        
      }
      
      dato_dish_ba_aggr[[s]] <- as.data.frame(bind_rows(dato_dish_ba[ s2:(s2 + length(datsets) - 1) ]))
      cat(s2, "\n")
      cat(s2:(s2 + length(datsets) - 1), "\n")
      
      s2 <- s2 + length(datsets)
      
    }
    
    # aggregating data
    dato_dish_ba_aggr <- 
      bind_rows(dato_dish_ba_aggr) %>%
      arrange(recipe_id) %>%
      mutate(nutrient = factor(nutrient, levels = gsub(x = nutrients_sorted, pattern = "_gt", replacement = ""), labels = nutrients_names_sorted),
             dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted)
      )
    
    unlink(x = "./support_data/list_complete_bland_altman*")
    write.xlsx(x = dato_dish_ba_aggr, file = "./support_data/bland-altman_dishes_complete.xlsx")
    
    
    # CREATING THE LIST OF OUT-of-LoAs DISHes
    dato_dish_ba <- list() #raccoglie la lista dei piatti overLOAs
    dato_dish_ba_aggr <- list() #lista finale
    
    n_data <- dir(path = "./support_data//")  
    n_data <- n_data[str_detect(string = n_data, pattern = "list_bland_altman") &
                       !str_detect(string = n_data, pattern = "~$*")
    ]
    n_data
    
    
    k <- 0
    s <- 0
    s2 <- 1
    
    for( i in n_data ){
      s <- s + 1
      
      for( j in datsets ){
        k <- k + 1
        
        dato_dish_ba[[k]] <- clean_names(read.xlsx(xlsxFile = paste0("./support_data/",i), sheet =  j))
        
        dato_dish_ba[[k]][,c("predicted", "observed")] <- apply(dato_dish_ba[[k]][, c("predicted", "observed") ], 2, as.numeric)
        
      }
      
      dato_dish_ba_aggr[[s]] <- as.data.frame(bind_rows(dato_dish_ba[ s2:(s2 + length(datsets) - 1) ]))
      
      #print(s2); print( s2:(s2 + length(datsets) - 1) )
      
      s2 <- s2 + length(datsets)
      
    }
    
    
    # JOINED DATASET WITH PLATES OUTSIDE THE LOAS AGGREGATED ACROSS DATASET
    dato_dish_ba_aggr <- 
      bind_rows(dato_dish_ba_aggr) %>% 
      arrange(recipe_id) %>%
      mutate(nutrient = factor( nutrient, levels = gsub(x = nutrients_sorted, pattern = "_gt", replacement = ""), labels = nutrients_names_sorted ),
             dataset = factor( dataset, levels = datsets_sorted, labels = data_names_sorted )
      ) %>%
      group_by_at(.vars = c("recipe_id", "dataset", "num_ingr_per_dish", "nutrient", "total_count")) %>%   # aggregate across models: by aggregating results over models, you may observe how estimates vary by dataset (interest for datascience of nutrition) while...
      summarise( predicted = mean(predicted),          # ...aggregate across datasets: by aggregating results over dataset, you may observe how estimates vary by model
                 observed = first(observed) 
      ) %>%  ungroup() %>%
      mutate(total_count = as.numeric(total_count)) %>%
      rename(total_count_across_model = total_count,
             mean_predicted_across_model = predicted) # count of the overLOAs dishes across by dataset --> AIM
    
    names(dato_dish_ba_aggr)
    dim(dato_dish_ba_aggr)
    head(dato_dish_ba_aggr, n  = 20)
    
    
    # table of out of LoAs
    k <- 0
    tabella <- list()
    
    for( i in levels(dato_dish_ba_aggr$dataset) ){
      k <- k + 1
      
      list_tab <- list(total_count_across_model = i,
                       mean_predicted_across_model = "Predicted mean value",
                       observed = "Observed mean value",
                       num_ingr_per_dish = "Number of ingredients"
      )
      
      
      tabella[[k]] <- 
        dato_dish_ba_aggr %>%
        filter(dataset == i) %>%
        select(!c("recipe_id", "dataset")) %>% 
        select(nutrient,
               total_count_across_model, num_ingr_per_dish, observed, mean_predicted_across_model
        ) %>%
        mutate(total_count_across_model = factor(cut(total_count_across_model,
                                                     breaks = c(0, 1, 2, 3)),
                                                 labels = c("1", "2", "3")
        )
        ) %>%
        tbl_summary(by = nutrient,
                    label = list_tab,
                    statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})"),
                                     all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = c(everything() ~ 1, all_categorical() ~ c(0, 1)),
        ) %>%  
        modify_header(label = "**Out-of-LOAs occurrences among FCDBs**",
                      all_stat_cols() ~ "**{level}**"  # don't show the "n" in the table
        ) %>% 
        italicize_levels() %>%
        modify_footnote(~NA) 
      
    }
    
    tabella_loas <- tbl_stack(tabella) %>% as_flex_table()
    tabella_loas
    
    if(doouts == 9999){
      cat("...printing table of Out-of-LOAS dishes only...")
      save_as_docx(tabella_loas, path = paste0("./tables/table_descriptive-outLOAs.docx"))
    }
    
    
    k <- 0
    tabella <- list()
    
    for( i in levels(dato_dish_ba_aggr$dataset) ){
      k <- k + 1
      
      list_tab <- list(total_count_across_model = i,
                       observed = "Observed mean value",
                       mean_predicted_across_model = "Predicted mean value",
                       num_ingr_per_dish = "Number of ingredients"
      )
      
      
      tabella[[k]] <- 
        dato_dish_ba_aggr %>%
        filter(dataset == i,
               total_count_across_model > 2) %>%
        select(!c("recipe_id", "dataset")) %>%
        relocate(any_of(c("total_count_across_model", "observed", "mean_predicted_across_model", "num_ingr_per_dish", "nutrient"))) %>%
        mutate(total_count_across_model = "") %>%
        tbl_summary(by = nutrient,
                    label = list_tab,
                    missing =  "no",
                    statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})"),
                                     all_categorical() ~ "{n} ({p}%)",
                                     total_count_across_model ~ c("n = {n}")
                    ),
                    digits = c(everything() ~ 1, all_categorical() ~ c(0, 1)),
        ) %>%  
        modify_header(label = "**Out-of-LOAs occurrences among FCDBs**",
                      all_stat_cols() ~ "**{level}**"  # remove the "n ="
        ) %>% 
        italicize_levels() %>%
        modify_footnote(~NA) 
      
    }
    
    tabella_outloas <- tbl_stack(tabella) %>% as_flex_table()
    tabella_outloas
    
    
    if(doouts == 9999){
      cat("...printing table of Out-of-LOAS dishes with algorithm names...")
      save_as_docx(tabella_outloas, path = paste0("./tables/table_mispredicted_focus-outLOAs.docx"))
    }
    
    
    # CREATING TABLE TO FOCUS ON THE COUPLE NUTRIENT * DATASET
    test <- 
      dato_dish_ba_aggr %>%
      mutate(dataset = factor(dataset, labels = datsets_sorted),
             nutrient = factor(nutrient, labels = substr(nutrients_sorted, start = 1, stop = 3)),
             total_count_across_model = as.character(total_count_across_model)
      ) %>%
      pivot_wider(id_cols = "recipe_id",
                  names_from = c("dataset", "nutrient"),
                  names_sep = "&&",
                  values_from = total_count_across_model,
                  values_fill = list(total_count_across_model = NA)  # empty cells are "NA"
      ) %>% as.data.frame()
    names(test)
    
    test$filter1 = 0
    test$algorithm1 = NA
    test$filter3 = 0
    temp <- as.data.frame(bind_rows(dato_dish_ba))
    
    n_righe <- (nlevels(dato_dish_ba_aggr$dataset))*(nlevels(dato_dish_ba_aggr$nutrient))
    
    
    for(i in 1:nrow(test)){
      test$filter3[i] = if_else(sum(c("3") %in% as.character(test[i, 1 + 1:n_righe]))  > 0, 1, 0)
      
      if(sum(as.character(test[i, 1 + 1:n_righe]) %in% "1")/length(na.omit(as.numeric(test[i, 1 + 1:n_righe]))) == 1){
        test$filter1[i] = 1
      }
      
      if(sum(as.character(test[i, 1 + 1:n_righe]) %in% "1") > 0){
        test[i, 1 + which(test[i, 1 + 1:n_righe] == "1")] <- gsub(x = paste(as.character(test[i, 1 + which(test[i, 1 + 1:n_righe] == "1")]), temp[(temp$recipe_id == test$recipe_id[i])&(temp$total_count == 1), "model"], sep = "***"),
                                                                  pattern = "IN1k_", replacement = ""
        )
        test$algorithm1[i] <- gsub(x = paste(sort(temp[(temp$recipe_id == test$recipe_id[i])&(temp$total_count == 1), "model"]), collapse = ", "),
                                   pattern = "IN1k_", replacement = ""
        )
        
      }
      
    }
    
    col_order <- expand.grid(dataset = datsets_sorted, nutrient = substr(nutrients_sorted, start = 1, stop = 3)) %>%
      unite("col_name", dataset, nutrient, sep = "&&") %>%
      pull(col_name)
    
    col_order2 <- df_nutrition5k %>% select(almond_milk:zucchini) %>% colnames %>% sort
    
    col_algorithm <- names(table(df_fin_reportino3$model))
    
    temp <- c(col_algorithm, datsets_sorted)
    for(k in 1:length(temp)){
      test[, ncol(test) + 1] <- NA
      colnames(test)[length(test)] <- temp[k]
    }
    
    
    for(i in 1:nrow(test)){
      if(!is.na(test$algorithm1[i])){
        temp <- unlist(strsplit(x = test$algorithm1[i], split = ", "))
        for(j in temp){
          test[i, j] <- 1
        }
        
        temp <- test[i, 1 + which(!is.na(test[i, 1 + 1:n_righe]))]
        temp <- temp[str_detect(string = temp, pattern = "1" )]
        
        data_chosed <- 
          datsets_sorted[str_detect(string = paste(x = names(temp), collapse = ", "), pattern = datsets_sorted)]
        
        for( k in data_chosed){
          test[i, k] <- paste(x = str_replace(string = temp[which( str_detect(string = names(temp), pattern = k) )],
                                              pattern = fixed("1***", ignore_case = TRUE), replacement = ""),collapse = ", "
          )
          
        }
      }
    }
    
    
    dato_dish_aggr_focused <- test %>% select(c("recipe_id", any_of(col_order), "filter1", "filter3", "algorithm1", any_of(col_algorithm), any_of(datsets_sorted))) %>%
      left_join(y = df_nutrition5k, by = "recipe_id") %>%
      rename_with(~ paste0(., "_1alg"), any_of(datsets_sorted)) %>%
      relocate(any_of(col_order2), .after = "ingredients")
    
    
    if(doouts == 9999){
      cat("...creatin data with Out-of-LOAs mispredicted dishes")
      write.xlsx(x = dato_dish_aggr_focused, file = "./support_data/list_dish_complete_mispredicted_ba.xlsx")
    }
    
    # UNIQUE LIST OF MISPREDICTED DISHes ACCORDING TO LoAs
    unique_dishes_outloas <- unique(subset(dato_dish_ba_aggr, dato_dish_ba_aggr$total_count_across_model > 2)$recipe_id)
    
    
    ##### 2bb - List of Dishes out-of-95% #####
    
    dato_dish_quantile <- list() #dataset by sheet
    dato_dish_quantile_aggr <- list() # merging list
    
    n_data <- dir(path = "./support_data/")  
    n_data <- n_data[str_detect(string = n_data, pattern = "list_complete_quantile")]
    n_data
    
    k <- 0
    s <- 0
    s2 <- 1
    
    for( i in n_data ){
      s <- s + 1
      
      for( j in datsets ){
        k <- k + 1
        
        dato_dish_quantile[[k]] <- clean_names(read.xlsx(xlsxFile = paste0("./support_data/",i), sheet =  j))
        
        dato_dish_quantile[[k]][,c("predicted", "observed")] <- apply(dato_dish_quantile[[k]][,c("predicted", "observed")], 2, as.numeric)
        
      }
      
      dato_dish_quantile_aggr[[s]] <- as.data.frame(bind_rows(dato_dish_quantile[ s2:(s2 + length(datsets) - 1) ]))
      print(s2)
      print(s2:(s2 + length(datsets) - 1))
      s2 <- s2 + length(datsets)
      
    }
    
    # aggregated data
    dato_dish_quantile_aggr <-
      bind_rows(dato_dish_quantile_aggr) %>%
      arrange(recipe_id) %>%
      mutate(nutrient = factor(nutrient, levels = gsub(x = nutrients_sorted, pattern = "_gt", replacement = ""), labels = nutrients_names_sorted),
             dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted)
      )
    
    unlink(x = "./support_data/list_complete_quantile*")
    write.xlsx(x = dato_dish_quantile_aggr, file = "./support_data/dishes_quantile_complete.xlsx")
    
    
    # CREATING THE LIST OF OUT-of-95th QUANTILE DISHes
    dato_dish_quantile <- list() # >95th-quantile list by nutrient (xlsx file) and dataset (sheet)
    dato_dish_quantile_aggr <- list() # final list
    
    n_data <- dir(path = "./support_data/")  
    n_data <- n_data[str_detect(string = n_data, pattern = "list_quantile")]
    n_data
    
    k <- 0
    s <- 0
    s2 <- 1
    
    for( i in n_data ){
      s <- s + 1
      
      for( j in datsets ){
        k <- k + 1
        
        dato_dish_quantile[[k]] <- clean_names(read.xlsx(xlsxFile = paste0("./support_data/",i), sheet =  j))
        
        dato_dish_quantile[[k]][,c("predicted", "observed")] <- apply(dato_dish_quantile[[k]][,c("predicted", "observed")], 2, as.numeric)
        
      }
      
      dato_dish_quantile_aggr[[s]] <- as.data.frame(bind_rows(dato_dish_quantile[ s2:(s2 + length(datsets) - 1) ]))
      
      # print(s2); print(s2:(s2 + length(datsets) - 1))
      
      s2 <- s2 + length(datsets)
      
    }
    
    
    # JOINED DATASET WITH PLATEs OUTSIDE THE LOAs AGGREGATED ACROSS DATASET
    dato_dish_quantile_aggr <- 
      bind_rows(dato_dish_quantile_aggr) %>%
      arrange(recipe_id) %>%
      mutate(nutrient = factor(nutrient, levels = gsub(x = nutrients_sorted, pattern = "_gt", replacement = ""), labels = nutrients_names_sorted),
             dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted)
      ) %>%
      group_by_at(.vars = c("recipe_id", "dataset", "num_ingr_per_dish", "nutrient", "total_count")) %>%              # aggregate across models: by aggregating results over models, you may observe how estimates vary by dataset (interest for datascience of nutrition) while
      summarise(predicted = mean(predicted),          # ...aggregate across datasets: by aggregating results over datasets, you may observe how estimates vary by model
                observed = first(observed)) %>%
      ungroup() %>%
      mutate(total_count = as.numeric(total_count)) %>%
      rename(total_count_across_model = total_count,
             mean_predicted_across_model = predicted) # count of the overQNTs dishes across by dataset --> AIM
    
    names(dato_dish_quantile_aggr)
    dim(dato_dish_quantile_aggr)
    head(dato_dish_quantile_aggr, n  = 20)
    
    
    # table of out of 95%-quantile
    k <- 0
    tabella <- list()
    
    for(i in levels(dato_dish_quantile_aggr$dataset)){
      k <- k + 1
      
      list_tab <- list(total_count_across_model = i,
                       mean_predicted_across_model = "Predicted mean value",
                       observed = "Observed mean value",
                       num_ingr_per_dish = "Number of ingredients"
      )
      
      
      tabella[[k]] <- 
        dato_dish_quantile_aggr %>%
        filter(dataset == i) %>%
        select(!c("recipe_id", "dataset")) %>% 
        select(nutrient,
               total_count_across_model, num_ingr_per_dish, observed, mean_predicted_across_model
        ) %>%
        mutate(total_count_across_model = factor(cut(total_count_across_model,
                                                     breaks = c(0, 1, 2, 3)),
                                                 labels = c("1", "2", "3")
        )
        ) %>%
        tbl_summary(by = nutrient,
                    label = list_tab,
                    statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})"),
                                     all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = c(everything() ~ 1, all_categorical() ~ c(0, 1)),
        ) %>%  
        modify_header(label = "**Out-of-95% occurrences among FCDBs**",
                      all_stat_cols() ~ "**{level}**"  # remove the "n ="
        ) %>% 
        italicize_levels() %>%
        modify_footnote(~NA) 
      
    }
    
    tabella_quantile <- tbl_stack(tabella) %>% as_flex_table()
    tabella_quantile
    
    #not print with != c(0, 1)
    if(doouts == 9999){
      cat("...printing table with out-of 95%quantile mispredicted dishes")
      save_as_docx(tabella_quantile, path = paste0("./tables/table_descriptive-out95.docx"))
    }
    
    
    k <- 0
    tabella <- list()
    
    for(i in levels(dato_dish_quantile_aggr$dataset)){
      k <- k + 1
      
      list_tab <- list(total_count_across_model = i,
                       observed = "Observed mean value",
                       mean_predicted_across_model = "Predicted mean value",
                       num_ingr_per_dish = "Number of ingredients"
      )
      
      
      tabella[[k]] <- 
        dato_dish_quantile_aggr %>%
        filter(dataset == i,
               total_count_across_model > 2) %>%
        select(!c("recipe_id", "dataset")) %>%
        relocate(any_of(c("total_count_across_model", "observed", "mean_predicted_across_model", "num_ingr_per_dish", "nutrient"))) %>%
        mutate(total_count_across_model = "") %>%
        tbl_summary(by = nutrient,
                    label = list_tab,
                    missing =  "no",
                    statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})"),
                                     all_categorical() ~ "{n} ({p}%)",
                                     total_count_across_model ~ c("n = {n}")
                    ),
                    digits = c(everything() ~ 1, all_categorical() ~ c(0, 1)),
        ) %>%  
        modify_header(label = "**Out-of-95% occurrences among FCDBs**",
                      all_stat_cols() ~ "**{level}**"  # remove the "n ="
        ) %>% 
        italicize_levels() %>%
        modify_footnote(~NA) 
      
    }
    
    tabella_outquantile <- tbl_stack(tabella) %>% as_flex_table()
    tabella_outquantile
    
    #not print with != c(0, 1)
    if(doouts == 9999){
      cat("...printing table with out-of 95%quantile mispredicted dishes with algorithm names...")
      save_as_docx(tabella_outquantile, path = paste0("./tables/table_focus-out95.docx"))
    }
    
    
    # CREATING TABLE TO FOCUS ON THE COUPLE NUTRIENT * DATASET
    test <- 
      dato_dish_quantile_aggr %>%
      mutate(dataset = factor(dataset, labels = datsets_sorted),
             nutrient = factor(nutrient, labels = substr(nutrients_sorted, start = 1, stop = 3)),
             total_count_across_model = as.character(total_count_across_model)
      ) %>%
      pivot_wider(id_cols = "recipe_id",
                  names_from = c("dataset", "nutrient"),
                  names_sep = "&&",
                  values_from = total_count_across_model,
                  values_fill = list(total_count_across_model = NA)  # Le celle vuote saranno NA
      ) %>% as.data.frame()
    names(test)
    
    test$filter1 = 0
    test$algorithm1 = NA
    test$filter3 = 0
    temp <- as.data.frame(bind_rows(dato_dish_quantile))
    
    n_righe <- (nlevels(dato_dish_quantile_aggr$dataset))*(nlevels(dato_dish_quantile_aggr$nutrient))
    
    
    for(i in 1:nrow(test)){
      test$filter3[i] = if_else(sum(c("3") %in% as.character(test[i, 1 + 1:n_righe]))  > 0, 1, 0)
      
      if(sum(as.character(test[i, 1 + 1:n_righe]) %in% "1")/length(na.omit(as.numeric(test[i, 1 + 1:n_righe]))) == 1){
        test$filter1[i] = 1
      }
      
      if(sum(as.character(test[i, 1 + 1:n_righe]) %in% "1") > 0){
        test[i, 1 + which(test[i, 1 + 1:n_righe] == "1")] <- gsub(x = paste(as.character(test[i, 1 + which(test[i, 1 + 1:n_righe] == "1")]), temp[(temp$recipe_id == test$recipe_id[i])&(temp$total_count == 1), "model"], sep = "***"),
                                                                  pattern = "IN1k_", replacement = ""
        )
        
        test$algorithm1[i] <- gsub(x = paste(sort(temp[(temp$recipe_id == test$recipe_id[i])&(temp$total_count == 1), "model"]), collapse = ", "),
                                   pattern = "IN1k_", replacement = ""
        )
        
      }
      
    }
    
    
    temp <- c(col_algorithm, datsets_sorted)
    for(k in 1:length(temp)){
      test[, ncol(test) + 1] <- NA
      colnames(test)[length(test)] <- temp[k]
    }
    
    
    for(i in 1:nrow(test)){
      if(!is.na(test$algorithm1[i])){
        temp <- unlist(strsplit(x = test$algorithm1[i], split = ", "))
        for(j in temp){
          test[i, j] <- 1
        }
        
        temp <- test[i, 1 + which(!is.na(test[i, 1 + 1:n_righe]))]
        temp <- temp[str_detect(string = temp, pattern = "1" )]
        
        data_chosed <- 
          datsets_sorted[str_detect(string = paste(x = names(temp), collapse = ", "), pattern = datsets_sorted)]
        
        for( k in data_chosed){
          test[i, k] <- paste(x = str_replace(string = temp[which( str_detect(string = names(temp), pattern = k) )],
                                              pattern = fixed("1***", ignore_case = TRUE), replacement = ""),collapse = ", "
          )
          
        }
      }
    }
    
    
    dish_list_quantile_aggr_focused <- test %>% select(c("recipe_id", any_of(col_order), "filter1", "filter3", "algorithm1", any_of(col_algorithm), any_of(datsets_sorted))) %>%
      left_join(y = df_nutrition5k, by = "recipe_id") %>%
      rename_with(~ paste0(., "_1alg"), any_of(datsets_sorted)) %>%
      relocate(any_of(col_order2), .after = "ingredients")
    
    # UNIQUE LIST OF MISPREDICTED DISHes ACCORDING TO 95th-QUANTILE
    unique_dishes_quantile <- unique(subset(dato_dish_quantile_aggr, dato_dish_quantile_aggr$total_count_across_model > 2)$recipe_id)
    
    
    # intersectio across lists (out-of-LOAs && 95pct) by the two methods (filter on the list and filter on the algorithm)
    unique_mispredicted_dishes <- list()
    unique_mispredicted_dishes[[1]] <- intersect(x = dato_dish_aggr_focused[dato_dish_aggr_focused$filter3 == 1, "recipe_id"], y = dish_list_quantile_aggr_focused[dish_list_quantile_aggr_focused$filter3 == 1, "recipe_id"])
    unique_mispredicted_dishes[[2]] <- intersect(x = unique_dishes_outloas, y = unique_dishes_quantile)
    
    unique_mispredicted_dishes[[1]] == unique_mispredicted_dishes[[2]]  # proof
    unique_mispredicted_dishes <- unique_mispredicted_dishes[[1]]
    
    mispred_reportino3 <- dato_dish_ba_aggr[dato_dish_ba_aggr$recipe_id %in% unique_mispredicted_dishes, ]
    
    
    # tabella piatti fuori dai LoAs e >95%
    k <- 0
    tabella <- list()
    
    for( i in levels(mispred_reportino3$dataset) ){
      k <- k + 1
      
      list_tab <- list(total_count_across_model = i,
                       mean_predicted_across_model = "Predicted mean value",
                       observed = "Observed mean value",
                       num_ingr_per_dish = "Number of ingredients"
      )
      
      
      tabella[[k]] <- 
        mispred_reportino3 %>%
        filter(dataset == i,
               total_count_across_model == 3) %>%
        select(!c("recipe_id", "dataset")) %>% 
        select(nutrient, total_count_across_model, num_ingr_per_dish, observed, mean_predicted_across_model
        ) %>%
        mutate(total_count_across_model = as.factor(total_count_across_model)
        ) %>%
        tbl_summary(by = nutrient,
                    label = list_tab,
                    statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})"),
                                     all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = c(everything() ~ 1, all_categorical() ~ c(0, 1)),
        ) %>%  
        modify_header(label = "**Out-of-95% occurrences among FCDBs**",
                      all_stat_cols() ~ "**{level}**"  # remove the "n ="
        ) %>% 
        italicize_levels() %>%
        modify_footnote(~NA) 
      
    }
    
    tabella_mispredicted <- tbl_stack(tabella) %>% as_flex_table()
    tabella_mispredicted
    
    
    mispred_reportino3 <- merge( x = mispred_reportino3[mispred_reportino3$total_count_across_model == 3, ], y = df_nutrition5k[df_nutrition5k$recipe_id %in% unique_mispredicted_dishes, c("recipe_id", "ingredients", "num_ingr_per_dish")], all.x = T )
    
    problematic_dishes <- c("dish_1563566909", "dish_1566414291", "dish_1566501575", "dish_1566501594", "dish_1566589933", "dish_1563566939", "dish_1563566965", "dish_1558630325", "dish_1558720236", "dish_1562703447", "dish_1563389626", "dish_1566838351")
    
    temp <- paste(wd_main, wd_input_data, "reportino2_rerun_3alg/support_data", sep = "/")
    
    wb <- createWorkbook()
    
    if( "dishes_mispredicted_reportino2.xlsx" %in% dir(temp) ){
      
      mispred_reportino2 <- read.xlsx( xlsxFile = paste(temp, "dishes_mispredicted_reportino2.xlsx", sep = "/"), sheet = "reportino2_list_unique_dishes" )
      mispred_reportino2 <- data.frame(recipe_id = unique(mispred_reportino2$recipe_id))
      mispred_reportino2$problematic_dish <- ifelse(mispred_reportino2$recipe_id %in% problematic_dishes, 1, 0)
      mispred_reportino2$no_common_rep3 <- ifelse( (! mispred_reportino2$recipe_id %in% unique(mispred_reportino3$recipe_id)), 1, 0)
      non_probl_dishes_rep2 <- mispred_reportino2$recipe_id[mispred_reportino2$problematic_dish == 0]
      
      if(doouts == 1){
        
        cat("...printing table and data...\n")
        
        addWorksheet(wb = wb, sheetName = "reportino2_list_unique_dishes")
        writeData(wb = wb, sheet = "reportino2_list_unique_dishes", x = mispred_reportino2, startRow = 1, startCol = 1)
        
        save_as_docx(tabella_mispredicted, path = paste0("./tables/table_mispredicted_ba_95pct.docx"))
        
      }
      
    }else{
      
      mispred_reportino2 <- NULL
      non_probl_dishes_rep2 <- NULL
      
    }
    
    mispred_reportino3$problematic_dish <- ifelse(mispred_reportino3$recipe_id %in% problematic_dishes, 1, 0)
    
    unique_reportini <- intersect(x = mispred_reportino3$recipe_id, y = mispred_reportino2$recipe_id)
    
    mispred_reportino3$new_entries <- ifelse( (! mispred_reportino3$recipe_id %in% mispred_reportino2$recipe_id) & (mispred_reportino3$problematic_dish == 0), 1, 0)
    
    mispred_common_reportini <- df_nutrition5k[df_nutrition5k$recipe_id %in% unique_reportini, c("recipe_id", "ingredients", "num_ingr_per_dish")]
    
    mispred_common_reportini$problematic_dish <- ifelse(mispred_common_reportini$recipe_id %in% problematic_dishes, 1, 0)
    
    
    if(doouts == 1){
      
      cat("...printing data with mispredicted dishes...\n")
      
      addWorksheet(wb = wb_ba, sheetName = "list unique reportino3")
      writeData(wb = wb_ba, sheet = "list unique reportino3", x = mispred_reportino3, startRow = 1, startCol = 1)
      
      addWorksheet(wb = wb_ba, sheetName = "list unique common")
      writeData(wb = wb_ba, sheet = "list unique common", x = mispred_common_reportini, startRow = 1, startCol = 1)
      
      saveWorkbook(wb = wb, file = "./support_data/dishes_mispredicted_reportino3.xlsx", overwrite = TRUE)
      
    }
    
    mispred_reportino3_list[[ which( cartel == cartels ) ]] <- mispred_reportino3
    names(mispred_reportino3_list)[[ which( cartel == cartels ) ]] <- cartel
    
  }
  
  
  #### 3 - INGREDIENTs PERFORMANCEs ####
  cat("\n ######### INGREDIENTs PERFORMANCEs ########## \n")
  
  ##### 3a - total number of ingredient detection #####
  
  test <-
    df_fin_reportino3 %>%
    group_by(recipe_id, model, dataset) %>%
    select( !contains( c("_quart", "_quint", apply(expand.grid(c("mass", "cal", "fat", "prot", "carb"), c("pred", "gt")), 1, paste, collapse="_")) ) ) %>%
    distinct() %>% ungroup() %>% as_data_frame()
  
  
  names(test)
  head(test)
  dim(test)
  dim(test)[1]/(length(names(table(test$model)))*length(names(table(test$dataset)))) #should be 676
  
  
  tabella <- subset(test, (test$dataset == "ita_corr") ) %>%
    select(apples_with_peel_pred:zucchini_pred) %>%
    summarise_all(.funs = list(Min = ~ min(., na.rm = T),
                               Q1 = ~ quantile(., 0.25, na.rm = T),
                               Me = ~ median(., na.rm = T),
                               Q3 = ~ quantile(., 0.75, na.rm = T),
                               Max = ~ max(., na.rm = T),
                               Mean = ~ mean(., na.rm = T),
                               SD = ~ sd(., na.rm = T)
    )
    ) %>%
    round(., 2) %>%
    pivot_longer(everything(), names_sep='_pred_', names_to=c('.value', 'statistics')) 
  
  
  tabella %>% regulartable() %>% autofit()
  
  tabella2 <- tabella[, c("statistics", top_ingr)]
  
  tabella <- tabella %>% regulartable() %>% autofit()
  
  tabella2 <- tabella2 %>% regulartable() %>% autofit()
  
  
  if(doouts == 9999){
    cat("...printing tables with mean-probabilities of all and top30 ingredients across 5 runs...")
    save_as_docx(tabella, path = paste0("./tables/table-meanprobs.docx"))  
    save_as_docx(tabella2, path = paste0("./tables/table-meanprobs_top30.docx"))  
  }    
  
  
  test2 <-
    test %>%
    rowwise() %>%
    mutate( across(ends_with("_pred"),  ~ round( ., 0) ), # rounding "probabilities"
            num_ingr_per_dish = if_else(is.na(num_ingr_per_dish), 0, num_ingr_per_dish), # removing NA for plate_only
            ingredients = if_else(is.na(ingredients), "plate_only", ingredients),  # removing NA for ingredients column in plate_only
            somma_pred = sum(c_across(contains("_pred")), na.rm = T),
            somma_gt = sum(c_across(contains("_gt")), na.rm = T)
    ) %>%
    mutate( somma_gt = if_else(recipe_id == "dish_1557861216", 0, somma_gt) )
  
  
  cat( "summary of # ingredients predicted: \n"); print(summary(test2$somma_pred))
  cat( "summary of # ingredients observed (calculated from new data: \n"); print(summary(test2$somma_gt))
  cat( "summary of # ingredients observed (from nutrition5k): \n"); print(summary(test2$num_ingr_per_dish))
  
  
  #dim(test2)[1]/(length(table(test2$model))*length(table(test2$dataset)))
  test[test$recipe_id == "dish_1557861216", "num_ingr_per_dish"]
  test2[test2$recipe_id == "dish_1557861216", c("num_ingr_per_dish", "ingredients", "somma_gt", "num_ingr_per_dish")]
  
  
  table(round(test2$somma_pred) == test2$num_ingr_per_dish)
  table(test2$somma_gt == test2$num_ingr_per_dish) # should be always true
  righe <- which(!test2$somma_gt == test2$num_ingr_per_dish) # sum of ingredient = a priori (B24) number of ingr per dish, should be null
  righe
  
  
  nomi_ingr <- df_fin_reportino3 %>%
    select(ends_with("_gt") & !contains(c(nutrients))) %>%
    rename_all(.funs = ~gsub(., pattern = "_gt", replacement = "")) %>%
    names()
  
  
  df_fin_reportino3_ingr <- 
    test2 %>% 
    mutate( num_ingr_per_dish_cat = cut(num_ingr_per_dish, breaks = c( min(test2$num_ingr_per_dish, na.rm = T), 2, 4, 10, max(test2$num_ingr_per_dish, na.rm = T) ), include.lowest = T, right = F),
            somma_diff = somma_pred - somma_gt,
            dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted)
    ) %>%
    mutate(across(almonds_pred:zucchini_gt, ~ factor( ., levels = c("0", "1") ) )) %>%
    as.data.frame()
  
  
  head(cbind(df_fin_reportino3_ingr[ ,c("somma_pred", "somma_gt", "somma_diff"), ], "estimated == observed" = df_fin_reportino3_ingr$somma_gt == df_fin_reportino3_ingr$num_ingr_per_dish))
  
  names(df_fin_reportino3_ingr)
  
  
  righe <- which(is.na(df_fin_reportino3_ingr), arr.ind = T)[, 1]; righe #should be null
  head(df_fin_reportino3_ingr[righe, c("recipe_id", "ingredients")])
  
  
  temp <- df_fin_reportino3_ingr %>% 
    filter(dataset == data_names_sorted[1] & model == "R101_2+2") %>%
    select(paste0(top_ingr, "_gt")) %>%
    mutate(across(contains("_gt"), ~ as.numeric(.) - 1))
  
  
  temp <- data.frame(ingredient = factor(top_ingr, levels = top_ingr, labels = firstup(gsub(x = top_ingr, pattern = "_", replacement = " "))),
                     perc = 100 * as.numeric(colMeans(temp))
  )
  
  p <-
    ggplot(data = temp) +
    geom_bar(mapping = aes(y = perc, x = ingredient), stat = "identity", fill = "red4") +
    theme_minimal(base_size = 28) +
    scale_y_continuous(name = "Percentage of ingredients", n.breaks = 10, labels = scales::percent_format(scale = 1)) +
    theme(axis.text.x = element_text(size = 16, angle = 30, hjust = 1, vjust = 1.3),
          axis.title.y = element_blank(),
          strip.text = element_text(size = 20),
          axis.ticks = element_blank(),
          legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Spectral")
  
  p
  
  
  if(doouts == 9999){
    
    ggsave2(
      filename = paste0("./figures/top30.pdf"),
      plot = p,
      width = 20, height = 10,
      scale = 2,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
  
  df_fin_reportino3_ingr_list[[ which( cartel == cartels ) ]] <- df_fin_reportino3_ingr
  names(df_fin_reportino3_ingr_list)[ which( cartel == cartels ) ] <- cartel
  
  
  ##### 3b - single performances and confusion matrices #####
  
  test <- df_fin_reportino3_ingr 
  
  test2 <- df_fin_reportino3 %>%
    select_at( c( "recipe_id", "model", "dataset", "num_ingr_per_dish", paste0(nomi_ingr,"_gt") ) ) %>%
    pivot_longer(
      cols = ends_with("_gt"),
      names_to = c("ingredient"),
      values_to = "n"
    ) %>%
    filter(n == 1)  %>%
    apply(2, function(x) gsub('_gt', "", x)) %>%
    as.data.frame() %>%
    mutate(dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted),
           num_ingr_per_dish = as.numeric(num_ingr_per_dish),
           n = as.numeric(n)) %>%
    group_by(model, dataset, ingredient) %>%
    summarise( median = median(num_ingr_per_dish, na.rm = T),
               q1 = quantile(num_ingr_per_dish, probs = 0.25, na.rm = T),
               q3 = quantile(num_ingr_per_dish, probs = 0.75, na.rm = T) ) %>% 
    relocate(ingredient, .before = model) %>% as.data.frame()
  
  
  test2$dishes <- NA
  
  k <- 0
  for (i in nomi_ingr) {
    k <- k + 1
    test2[test2$ingredient %in% i, "dishes"] <- paste(unique(df_fin_reportino3[df_fin_reportino3[, paste0(i, "_gt") ] == 1, c("recipe_id")]), collapse = ", ")
  }
  
  
  conf_mat <- list()
  cutoff_low <- 0.2
  cutoff_up <- 0.8
  
  dishes_id <- unique(df_fin_reportino3_ingr$recipe_id)
  conf_mat_names <- c("ingredient", "dataset", "model", "TP", "FN", "FP", "TN","Precision", "Recall", "BalAcc", "F1", paste( "F1", round(cutoff_low, 1), round(cutoff_up, 1), sep = "_" ), "q1_dish_size", "median_dish_size", "q3_dish_size", "dishes", dishes_id)
  
  conf_matrix <- data.frame(matrix(NA, ncol = length(conf_mat_names)))
  names(conf_matrix) <-  conf_mat_names
  
  
  s_init <- 1
  s_end <-length(nomi_ingr)
  t <- 0
  
  
  for(i in names(table(test$dataset))){
    
    r <- 0
    t <- t + 1
    
    cat("Dataset:", data_names_sorted[t], "\n")
    
    conf_mat[[t]] <- list()
    names(conf_mat)[t] <- datsets_sorted[t]
    
    
    for(j in names(table(test$model))){
      
      cat("Model:", j, "\n")
      
      #paste0("Data: ",data_names[t] ,";   Model: '", j, "'") #, "'; dataset '", data_names[i], "'") 
      title <- j 
      r <- r + 1
      k <- 0
      
      conf_mat[[t]][[r]] <- list()
      names(conf_mat[[t]])[r] <- j
      
      
      subdata <- row.names(test)[(test$model == j) & (test$dataset == i)]
      
      
      if(length(subdata) == 0){
        
        cat("######## data unavailable ######## \n")
        
      }else{
        
        conf_matrix[s_init:s_end, 1] <- nomi_ingr
        conf_matrix[s_init:s_end, 2] <- i
        conf_matrix[s_init:s_end, 3] <- j
        
        
        for(s in nomi_ingr){
          
          #print(paste("k = ", k))
          k <- k + 1
          row_mat <- s_init + k - 1
          
          subdata2 <- (which((test2$model == j) & (test2$dataset == i) & (test2$ingredient == s)))
          temp2 <- rep(NA, 3)
          
          if(length(subdata2) > 0){
            
            temp2[1] <- test2[subdata2, "q1"]
            temp2[2] <- test2[subdata2, "median"]
            temp2[3] <- test2[subdata2, "q3"]
            
            for(l in 1:length(temp2)){
              temp2[l] <- ifelse(temp2[l] == 0, NA, as.numeric(temp2[l]))
            }
            
          } 
          
          conf_matrix[row_mat, 13] <- temp2[1]
          conf_matrix[row_mat, 14] <- temp2[2]
          conf_matrix[row_mat, 15] <- temp2[3]
          
          temp <- table("Observed" = test[ subdata, paste0(s,"_gt")], "Predicted" = test[subdata, paste0(s,"_pred")])
          temp <- confusionMatrix(temp, mode = "everything", positive = "1")
          
          conf_mat[[t]][[r]][[k]] <- temp  
          names(conf_mat[[t]][[r]])[k] <- s
          
          
          temp1 <- temp$table
          
          
          conf_matrix[row_mat, 7] <- temp1[1,1] #TN
          conf_matrix[row_mat, 6] <- temp1[1,2] #FP
          conf_matrix[row_mat, 5] <- temp1[2,1] #FN
          conf_matrix[row_mat, 4] <- temp1[2,2] #TP
          
          conf_matrix[row_mat, 8] <- temp1[2,2] / (temp1[2,2] + temp1[1,2]) # Precision: TP/ MODPOS
          conf_matrix[row_mat, 9] <- temp1[2,2] / (temp1[2,2] + temp1[2,1]) # Recall: Sensitivity - TPR
          conf_matrix[row_mat, 10] <- (temp1[1,1] / sum(temp1[1, ]) + conf_matrix[row_mat, 9]) / 2 # Balanced accuracy
          conf_matrix[row_mat, 11] <- 2 * prod(conf_matrix[row_mat, 8:9]) / sum(conf_matrix[row_mat, 8:9])
          
          temp1 <- unique(test2[subdata2 , "dishes"])
          conf_matrix[row_mat, 16] <- ifelse(length(temp1) == 0, "not present", as.character(temp1))
          
          # possible constraints on F1
          temp1 <-  (is.na(conf_matrix[row_mat, 11])) & (sum(conf_matrix[row_mat, 4:6], na.rm = T) > 0) 
          if(temp1){ conf_matrix[row_mat, 11] <- 0 }
          
          conf_matrix[row_mat, 12] <- case_when(is.na(conf_matrix[row_mat, 11]) ~ "Ind_right", 
                                                conf_matrix[row_mat, 11] == 0 ~ "Ind_wrong",
                                                ( conf_matrix[row_mat, 11] >= cutoff_low ) & ( conf_matrix[row_mat, 11] <= cutoff_up )  ~  paste(cutoff_low, "x", cutoff_up, sep = "<="), 
                                                conf_matrix[row_mat, 11] < cutoff_low ~ paste0("<", cutoff_low),
                                                conf_matrix[row_mat, 11] > cutoff_up ~ paste0(">", cutoff_up)
                                                )
          
          temp2 <- NA
          for(m in dishes_id){
            
            temp2[1] <- which( (df_fin_reportino3_ingr$recipe_id %in% m) & (df_fin_reportino3_ingr$model %in% j) & (df_fin_reportino3_ingr$dataset == i) )
            temp2[2] <- as.numeric(df_fin_reportino3_ingr[temp2[1], paste0(s, "_pred")]) - 1
            temp2[3] <- as.numeric(df_fin_reportino3_ingr[temp2[1], paste0(s, "_gt")]) - 1
            
            conf_matrix[row_mat, m] <- temp2[2] - temp2[3]
            conf_matrix[row_mat, m] <- ifelse( conf_matrix[row_mat, m] < 0, "FlsNeg", ifelse(conf_matrix[row_mat, m] > 0, "FlsPos", ifelse(temp2[3] == 0, "TN", "TP") ) )
            
          }
          
        }
        
        
        s_init <- s_end + 1
        s_end <- s_end + length(nomi_ingr)
        
      }
      
    }
    
  }
  
  conf_matrix_list[[ which( cartel == cartels ) ]] <- conf_matrix
  names(conf_matrix_list)[[ which( cartel == cartels ) ]] <- cartel
  
  
  ingr_performances <-
    conf_matrix %>%
    group_by(dataset, ingredient) %>%
    mutate(across( starts_with("F1_"), .fns = ~ case_when(all( . == "Null" ) ~ 1, TRUE ~ 0), .names = "mispredicted" )
    ) %>%
    relocate("mispredicted", .after = starts_with("F1_")) %>%
    as.data.frame()
  
  
  data_ingr_performance_list[[ which( cartel == cartels ) ]] <- ingr_performances
  names(data_ingr_performance_list)[[ which( cartel == cartels ) ]] <- cartel
  
  if(doouts == 1){
    cat("...creating data with single ingredients performances \n")
    
    wb <- createWorkbook()
    addWorksheet(wb = wb, sheetName = "dataset wide")
    writeData(wb = wb, sheet = "dataset wide", x = ingr_performances, startRow = 1, startCol = 1)
    
  }
  
  
  # ingredients with missing F1, count by dataset and ingredient
  temp <- 
    ingr_performances %>% 
    filter( is.na(F1) ) %>%
    group_by( ingredient, dataset ) %>%
    select( ingredient:F1 ) %>%
    mutate( count = n() ) 
  
  temp1 <- unique(temp$ingredient) # ingredienti univoci
  
  temp2 <- # subset of ingredients which have the condition of missing over all models, by dataset
    temp %>%
    filter(count == 3)
  
  table(temp$ingredient, temp$dataset) 
  dim(table(temp$ingredient, temp$dataset) )  
  
  table(temp2$ingredient, temp2$dataset) 
  dim(table(temp2$ingredient, temp2$dataset) )  
  
  temp[which(!temp1 %in% temp2$ingredient),]$"ingredient"
  
  temp[temp$ingredient %in% temp[which(!temp1 %in% temp2$ingredient),]$"ingredient", ]
  
  
  unique_not_present_ingr <- ingr_performances %>%
    filter(dishes == "not present") %>%
    select(ingredient) %>%
    distinct(ingredient) %>%
    as.data.frame()
  
  
  if(doouts == 1){
    cat("...creating list of ingredients not present in the test set \n")
    addWorksheet(wb = wb, sheetName = "ingredients not in test set")
    writeData(wb = wb, sheet = "ingredients not in test set", x = unique_not_present_ingr, startRow = 1, startCol = 1)
    #write.xlsx2(x = unique_not_present_ingr, file = "./support_data/ingr_dish_performances.xlsx", sheetName = "ingredients not in test set", append = T)
  }
  
  
  ## ## do test: choose ingredient, detect related dishes and prevision by dataset and algorithm ## ##
  test = 0
  
  if(test == 1){
    
    temp <- ingr_performances[ingr_performances$ingredient == "almonds",]
    temp[, c("dataset", "model", "BalAcc", "F1", "dish_1558113477")]
    
    k <- NULL
    for( j in which(names(temp) %in% dishes_id) ){
      
      if("FlsNeg" %in% temp[, j]){
        k <- c(k, j)
      }
    }
    
    temp[, c(1:5, k)] #dishes_ids in columns should contain a FlsNeg over their values
    
    temp1 <- ingr_performances[(ingr_performances$ingredient == "pizza") & (ingr_performances$dataset == data_names_sorted[1]) & (ingr_performances$model == "ViT-B-16_2+2"), c(8:11, 17)]
    temp2 <- df_fin_reportino3_ingr[(df_fin_reportino3_ingr$dataset == data_names_sorted[1]) & (df_fin_reportino3_ingr$model == "ViT-B-16_2+2"), c("recipe_id", "pizza_gt", "pizza_pred")]
    
    confusionMatrix(table("observed" = temp2$pizza_gt, "predicted" = temp2$pizza_pred), positive = "1", mode = "everything")
    
    paste(temp2[which(temp2$pizza_gt == 1), "recipe_id"], sep = "", collapse = ", ") == temp1$dishes # list of dishes were pizza is present = cell with list (should be true)
    
  }
  ## ## end test ## ##
  
  
  dish_ingr_performances <-
    ingr_performances %>%
    as_tibble() %>%
    pivot_longer(
      cols = starts_with("dish_"),
      names_to = "recipe_id",
      values_to = "prediction"
    ) %>%
    left_join(y = df_fin_reportino3[, c("recipe_id", "ingredients", "num_ingr_per_dish")], by = "recipe_id", relationship = "many-to-many") %>%
    relocate(c("recipe_id", "ingredients", "num_ingr_per_dish"), .after = "ingredient") %>%
    ungroup() 
  
  
  if(doouts == 9999){
    write.xlsx(x = dish_ingr_performances %>% filter(prediction != "TN"), file =  "./tables/ingr_dish_performances_long.xlsx")
  }
  
  
  dish_ingr_performances_median <- 
    dish_ingr_performances %>%
    group_by_at( vars( ingredient:dataset, mispredicted) ) %>% 
    mutate(across(c("BalAcc", "F1"), ~ median(., na.rm = T)) ,
           prediction = names(which.max(table(prediction))) ) %>%
    rename_with(.cols = c("BalAcc", "F1"), .fn = ~ paste0(., "_median") ) %>% 
    select( !model:Recall ) %>%
    slice(1) %>% as.data.frame() %>%
    ungroup()
  
  
  if(doouts == 9999){
    addWorksheet(wb = wb, sheetName = "dataset long mediated")
    writeData(wb = wb, sheet = "dataset long mediated", x = dish_ingr_performances_median, startRow = 1, startCol = 1)
    #write.xlsx2(x = dish_ingr_performances_median, file = "./support_data/ingr_dish_performances.xlsx", sheetName = "dataset long mediated", append = T)
  }  
  
  
  dish_ingr_performances_metric <- 
    dish_ingr_performances_median %>%
    group_by(ingredient, dataset, mispredicted) %>% 
    summarise_at(.vars = c("F1_median","BalAcc_median"), 
                 .funs = list(#Min = ~min(., na.rm = T),
                   Q1 = ~quantile(., 0.25, na.rm = T),
                   Me = ~median(., na.rm = T),
                   Q3 = ~quantile(., 0.75, na.rm = T)
                   #Max = ~max(., na.rm = T),
                   #Mean = ~mean(., na.rm = T),
                   #SD = ~sd(., na.rm = T)
                 )) %>%
    pivot_longer(!c("ingredient", "dataset", "mispredicted"), names_to = 'metric', values_to = 'value') %>%
    separate(metric, into = c("metrics", "statistics"), sep = "_median_")  %>%
    as.data.frame()
  
  
  temp <- dish_ingr_performances_metric[(dish_ingr_performances_metric$dataset == data_names_sorted[1]) & (dish_ingr_performances_metric$metric == "F1") & (dish_ingr_performances_metric$statistics == "Me"),]
  temp <- temp[order(temp$value, decreasing = T), ]
  summary(temp$value)
  summary(temp[!temp$ingredient %in% unique_not_present_ingr$ingredient, "value"])
  
  temp[(!temp$ingredient %in% unique_not_present_ingr$ingredient) & (temp$value >= 0.8), c("ingredient")]
  temp[(!temp$ingredient %in% unique_not_present_ingr$ingredient) & (temp$value < 0.2), c("ingredient")]
  
  
  dish_ingr_performances_metric_all_data[[ which( cartel == cartels ) ]] <- dish_ingr_performances_metric
  names(dish_ingr_performances_metric_all_data)[ which( cartel == cartels ) ] <- cartel
  
  
  if(doouts == 1){
    cat("...creating data of mediated performances of ingredients \n")
    write.xlsx(x = dish_ingr_performances_metric, file = "./support_data/ingr_metric_performances.xlsx")
  }
  
  
  list_ingr_dish_mispredicted <- 
    dish_ingr_performances_median %>%
    filter(#dataset == names(table(ingr_performances_median$dataset))[2],
      mispredicted == 1,
      dishes != "non present") %>%
    group_by_at( c("ingredient", "recipe_id" , "dataset") ) %>%
    slice(1) %>% as.data.frame()
  
  
  if(doouts == 9999){
    addWorksheet(wb = wb, sheetName = "mispredicted ingredients")
    writeData(wb = wb, sheet = "mispredicted ingredients", x = list_ingr_dish_mispredicted, startRow = 1, startCol = 1)
    #write.xlsx2(x = list_ingr_dish_mispredicted, file = "./support_data/ingr_dish_performances.xlsx", sheetName = "mispredicted ingredients", append = T)
  }
  
  unique_list_ingr_dish_mispredicted <- 
    list_ingr_dish_mispredicted %>%
    group_by(dataset) %>%
    distinct(ingredient) %>%
    as.data.frame()
  
  if(doouts == 1){
    cat("...creating list of unique mispredicted dishes...\n")
    addWorksheet(wb = wb, sheetName = "list mispredicted ingredients")
    writeData(wb = wb, sheet = "list mispredicted ingredients", x = unique_list_ingr_dish_mispredicted, startRow = 1, startCol = 1)
    #write.xlsx2(x = unique_list_ingr_dish_mispredicted, file = "./support_data/ingr_dish_performances.xlsx", sheetName = "list mispredicted ingredients", append = T)
  }
  
  
}

# END OF THE CYCLE

setwd( paste(wd_main, wd_output_data, sep = "/") )

{
  
  for( j in 1:length(cartels) ){
    
    # contains data on target variables of dishes
    df_fin_reportino3_list[[j]]$scenario <- names(df_fin_reportino3_list)[j]
    
    # contains info on ingredients predicted for each dishe
    df_fin_reportino3_ingr_list[[j]]$scenario <- names(df_fin_reportino3_ingr_list)[j]
    
    # mispredicted list of dishes
    mispred_reportino3_list[[j]]$scenario <- names(mispred_reportino3_list)[j]
    
    # contains information on residuals of target variable by dish. Used for recalculate MAPE 
    dato_5runs_performances_list[[j]]$scenario <- names(dato_5runs_performances_list)[j] 
    
    # information on aggregated metrics of target variable by dataset and algorithm
    dato_5runs_perf_aggr_all_data[[j]]$scenario <- names(dato_5runs_perf_aggr_all_data)[j]
    
    # information on ingredient detection and relative confusion matrices and performances, and matched info with dishes
    # use data_ingr_performance_list for plots, conf_mat for single estimates
    conf_matrix_list[[j]]$scenario <- names(conf_matrix_list)[j]
    data_ingr_performance_list[[j]]$scenario <- names(data_ingr_performance_list)[j]
    
    # information on aggregated metrics of ingredient by dataset and algorithm
    dish_ingr_performances_metric_all_data[[j]]$scenario <- names(dish_ingr_performances_metric_all_data)[j]
    
  }
  
  # needed to create table of frames (table_2)
  training_dishes <- unique(dato_training_set$recipe_id)
  testing_dishes <- unique(bind_rows(df_fin_reportino3_list)$recipe_id)
  
  # sorting frame filtering scenarios for plots
  list_ff_order <- setNames( paste(c("After", "Before"), "frame filtering"), cartels )
  list_ff_order <- rev(list_ff_order)
  
}

#save.image(file = "reportino3_after_prediction_ingr_w-comments.RData")

'
{
### RUN TO START FROM THIS POINT
  wd_main = "C:/Users/ASUS/Desktop/Milano/reportino3"
  wd_output_data = "output_data"
  
  setwd( paste(wd_main, wd_output_data, sep = "/") )
  
  load(file = "reportino3_after_prediction_ingr_w-comments.RData")
}
'


#### 4 - INGREDIENTs PERFORMANCE by FRAME FILTERING scenario ####

if(doouts == 1){
  
  setwd(dir = paste(wd_main, wd_output_data, sep = "/") )
  dir.create( paste(wd_main, wd_output_data, "ingredient_recognition", sep = "/") )
  
  setwd( paste(wd_main, wd_output_data, "ingredient_recognition", sep = "/") )
  
  dir.create("./figures/")
  dir.create("./support_data/")
  dir.create("./tables/")
  
}


##### 4a - ingredients predictions by dataset and frame filtering scenario #####
df_fin_reportino3_ingr_list <- 
  df_fin_reportino3_ingr_list %>% 
  bind_rows() %>%
  filter( endsWith( model,("2+2") ) & grepl(x = dataset, pattern = "—correction", fixed = F) ) %>%
  mutate(model = as.factor(model),
         dataset = factor(dataset, levels = data_names_sorted),
         scenario = factor(scenario, levels = names(list_ff_order), labels = as.character(list_ff_order))
  )

#table of predicted number of ingredients
tabella <-list()

temp <- 
  df_fin_reportino3_ingr_list %>%
  select(model, dataset, scenario, somma_pred, num_ingr_per_dish) %>%
  pivot_longer(cols = c(somma_pred, num_ingr_per_dish), names_to = "name") 

tabella[[1]] <-
  temp %>%
  filter(name == "num_ingr_per_dish" & model == "IncV3_2+2") %>%
  select(!model) %>%
  tbl_strata(
    strata = scenario,
    .tbl_fun = ~ .x %>%
      tbl_continuous(
        variable = value,
        by = dataset,
        label = list(name ~ "observed values"),
        statistic = list(everything() ~ "{median} ({p25}, {p75})"),
        digits = list(everything() ~ 0)
      ) %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_header(all_stat_cols() ~ "**{level}**")
  ) %>%
  modify_header(label = "**Metric**") %>%
  modify_footnote(everything() ~ NA)

tabella[[2]] <-
  temp %>%
  filter(name == "somma_pred") %>%
  select(!name) %>%
  tbl_strata(
    strata = scenario,
    .tbl_fun = ~ .x %>%
      tbl_continuous(
        variable = value,
        by = dataset,
        statistic = list(everything() ~ "{median} ({p25}, {p75})"),
        digits = list(everything() ~ 0)
      ) %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_header(all_stat_cols() ~ "**{level}**")
  ) %>%
  modify_header(label = "**Metric**") %>%
  modify_footnote(everything() ~ NA)

tabella <- tbl_stack(tabella) %>% as_flex_table()

if(doouts == 1){
  cat("...printing table S1 of the distribution of the predicted number of ingredient...")
  save_as_docx(x = tabella, path = "./tables/table_s1_predicted-number-ingredients.docx")
}


# plot of residual vs actual number of ingredients
q <-   
  ggplot(data = df_fin_reportino3_ingr_list, mapping = aes(x = num_ingr_per_dish)) +
  geom_jitter(data = df_fin_reportino3_ingr_list, mapping = aes(y = somma_diff, col = model)) +
  geom_hline(yintercept = 0, col = "red3", lty = "dashed", size = .8) + 
  geom_smooth(method = "gam", 
              mapping = aes(x = num_ingr_per_dish, y = somma_diff, col = model),
              size = 0.4) + 
  facet_grid(dataset ~ scenario) +
  scale_y_continuous(limits = c(-25, 60)) +
  labs(x = "Observed number of ingredients", y = "Predicted - observed number of ingredients", col = "Algorithm") +
  guides(fill = guide_legend(title = "Algorithm", title.position = "left", nrow = 1, byrow = TRUE)) +
  theme_minimal(base_size = 18) +
  theme( strip.text.y = element_text(size = 12),
         axis.text.x = element_text(size = 12, hjust = 1, vjust = 1.3),
         legend.position = "bottom" ) +
  scale_color_manual(values = c("orange", "steelblue", "green3"))

q

if(doouts == 1){
  
  cat("...printing figure 1 of ingredients prediction vs oberved values...\n")
  
  par(mar = c(0, 0, 0, 0))
  
  ggsave2(
    filename = paste0("./figures/figure_1_ingredients-scenarios.pdf"),
    plot = q,
    width = 16, height = 10,
    scale = 2,
    units = c("cm"),
    dpi = 300,
    limitsize = TRUE
  )
  
  dev.off()
  
}


##### 4b - performances of ingredients #####

# ingredient matrix
conf_matrix_list <-
  conf_matrix_list %>%
  bind_rows() %>%
  mutate(ingredient = gsub(x = ingredient, pattern = "_", replacement = " "),
         dataset = factor(dataset, levels = data_names_sorted),
         model = as.factor(model),
         scenario = factor(scenario, levels = names(list_ff_order), labels = as.character(list_ff_order))
  ) %>% 
  relocate(scenario, .after = model) %>% as.data.frame()


# table with F1s
tabella_all_f1_num <- list()
tabella <- list()
s <- 0
k <- 0

for(i in levels(conf_matrix_list$scenario)){
  
  s <- s + 1
  
  for(j in levels(conf_matrix_list$dataset)){
    k <- k + 1
    
    temp <- conf_matrix_list %>%
      filter(dataset == j & scenario == i) %>%
      select(ingredient, model, F1)
    
    
    tabella[[k]] <- 
      imap("{mean}",
           ~ as.data.frame(temp) %>%
             tbl_continuous(by = model,
                            variable = F1,
                            statistic = ~.x,
                            label = list(ingredient = j),
                            digits = c(everything() ~ 2)
             ) %>%
             bold_labels() %>%
             italicize_levels() %>%
             modify_header(
               all_stat_cols() ~ "**{level}**") #%>%
      ) %>%
      tbl_merge(tab_spanner = FALSE) %>%
      modify_header(label = "**Dataset and ingredient**") %>%
      modify_footnote(~NA)
    
    for(m in names(tabella[[k]]$table_body)[str_detect(string = names(tabella[[k]]$table_body), pattern = "stat")]){
      temp2 <- apply(tabella[[k]]$table_body[ -1, m], 1, as.numeric)
      temp2[is.na(temp2)] <- "NaN_R"
      temp2[temp2 == 0] <- "NaN_W"
      temp2[ ! (temp2 %in% c("NaN_R", "NaN_W")) ] <- format(as.numeric(temp2[ ! (temp2 %in% c("NaN_R", "NaN_W")) ]), nsmall = 2)
      tabella[[k]]$table_body[ -1, m] <- temp2
    }
    
  }
  
  tabella_all_f1_num[[s]] <- tbl_stack(tabella[(k - (length(data_names)- 1)):k]) 
  
} # later on the table is completed and possibly put into output !!!(DO NOT REWRITE THE 's' PARAMETER)


## dataset of F1 scores (continuous and categorical) from single ingredients
data_ingr_performance_list <-
  data_ingr_performance_list %>%
  bind_rows() %>%
  rename(value = F1) %>%
  select(c(ingredient:model, value:dishes, scenario)) %>%
  mutate(ingredient = gsub(x = ingredient, pattern = "_", replacement = " "),
         dataset = factor(dataset, levels = data_names_sorted),
         scenario = factor(scenario, levels = names(list_ff_order), labels = as.character(list_ff_order)),
         model = as.factor(model),
         delta = NA, # later on it will be computed
         delta_cat = factor(NA, levels = c("REAL", "UP", "DOWN", "Const_R", "Const_W")) # later on it will be computed
  ) %>%
  arrange(desc(value)) %>%
  mutate(color = case_when((value < cutoff_low) ~ 'red4',
                           (value >= cutoff_low & value <= cutoff_up) ~ 'black',
                           (value > cutoff_up) ~ 'green4',
                           is.na(value) ~ 'steelblue3'
  ),
  ingredient2 = glue("<i ='color:{color}'>{ingredient}</i>"),
  ingredient3 = italic_fun(value = value, 
                           ingredient = ingredient,
                           cutoff_low = cutoff_low,
                           cutoff_up = cutoff_up
  )
  ) %>%
  as.data.frame()


# delta% of F1 score over singles ingredients
for (r in (data_ingr_performance_list$ingredient)) {
  
  for (j in levels(data_ingr_performance_list$dataset)) {
    
    for (m in levels(data_ingr_performance_list$model)) {
      
      righe <- (data_ingr_performance_list$ingredient %in% r) & (data_ingr_performance_list$dataset %in% j) & (data_ingr_performance_list$model %in% m)
      righe2 <- righe & data_ingr_performance_list$scenario %in% levels(data_ingr_performance_list$scenario)[2]
      
      data_ingr_performance_list[righe2, "delta"] <-
        100 * (data_ingr_performance_list[righe2 , "value"] - data_ingr_performance_list[righe & data_ingr_performance_list$scenario %in% levels(data_ingr_performance_list$scenario)[1], "value"]) /
        data_ingr_performance_list[righe & data_ingr_performance_list$scenario %in% levels(data_ingr_performance_list$scenario)[1], "value"]
      
      temp1 <- data_ingr_performance_list[righe & data_ingr_performance_list$scenario == levels(data_ingr_performance_list$scenario)[1], "F1_0.2_0.8"]  
      temp2 <- data_ingr_performance_list[righe & data_ingr_performance_list$scenario == levels(data_ingr_performance_list$scenario)[2], "F1_0.2_0.8"]  
      
      
      data_ingr_performance_list[righe2, "delta_cat"] <- case_when(temp1 == "Ind_right" & temp2 == "Ind_right" ~ "Const_R",
                                                                   temp1 == "Ind_right" & temp2 == "Ind_wrong" ~ "DOWN",
                                                                   temp1 == "Ind_wrong" & temp2 == "Ind_right" ~ "UP",
                                                                   temp1 == "Ind_wrong" & temp2 == "Ind_wrong" ~ "Const_W",
                                                                   temp1 == "Ind_wrong" & !( temp2 %in% c ("Ind_right", "Ind_wrong") ) ~ "UP",
                                                                   !( temp1 %in% c ("Ind_right", "Ind_wrong") ) & ( temp2 == "Ind_wrong" ) ~ "DOWN",
                                                                   !( temp1 %in% c ("Ind_right", "Ind_wrong") ) & !( temp2 %in% c ("Ind_right", "Ind_wrong") ) ~ "REAL"
      )
      
    }
    
  }
  
}

# continued: creation of performance table over single ingredients and relative delta%'s
s <- s + 1
k <- 0

tabella <- list()

for(j in levels(data_ingr_performance_list$dataset)){
  k <- k + 1
  
  temp <- data_ingr_performance_list %>%
    filter(dataset == j, scenario == levels(scenario)[2]) %>%
    select(ingredient, model, delta, delta_cat) %>%
    arrange(model)
  
  
  tabella[[k]] <- 
    imap("{mean}",
         ~ as.data.frame(temp) %>%
           select(!delta_cat) %>%
           tbl_continuous(by = model,
                          variable = delta,
                          statistic = ~.x,
                          label = list(ingredient = j),
                          digits = c(everything() ~ 2)
           ) %>%
           bold_labels() %>%
           italicize_levels() %>%
           modify_header(
             all_stat_cols() ~ "**{level}**") #%>%
    ) %>%
    tbl_merge(tab_spanner = FALSE) %>%
    modify_header(label = "**Dataset and ingredient**") %>%
    modify_footnote(~NA)
  
  temp1 <- names(tabella[[k]]$table_body)[str_detect(string = names(tabella[[k]]$table_body), pattern = "stat")]  
  temp2 <- tabella[[k]]$table_body[ -1, c("label", temp1) ]
  
  
  for(i in temp2$label){
    
    temp3 <- temp[temp$ingredient == i, "delta_cat"]
    
    for(m in 1:length(temp1)){
      if( ! (temp3[m] == "REAL") ){
        tabella[[k]]$table_body[ tabella[[k]]$table_body$label == i, temp1[m] ] <- temp3[m] 
      }
    }
    
  }
  
}

tabella_all_f1_num[[s]] <- tbl_stack(tabella[(k - (length(data_names)- 1)):k])

tabella_all_f1_num <- tbl_merge(tabella_all_f1_num, tab_spanner = c("**Before frame filtering**", "**After frame filtering**", "**ΔF1 (%)**")) %>% as_flex_table

if(doouts == 1){
  cat("...printing tables S4 and S5 (to be split) of performance of single ingredients...\n")
  save_as_docx(tabella_all_f1_num, path = "./tables/table_s4-s5_ingr_f1_performances.docx")
}


# plot of F1 metrics
t <- 0
point_level <- - 0.05

for (m in levels(data_ingr_performance_list$dataset)) { 
  
  q <- list()
  t <- t + 1
  r <- 0
  
  for (s in levels(data_ingr_performance_list$scenario)) {
    
    p <- list()
    r <- r + 1
    k <- 0
    
    for(j in levels(data_ingr_performance_list$model)){
      
      k <- k + 1
      
      temp <-
        data_ingr_performance_list[(data_ingr_performance_list$dataset == m) &
                                     #!is.na(data_ingr_performance_list$value) &
                                     (data_ingr_performance_list$model == j) &
                                     (data_ingr_performance_list$scenario == s),
        ] %>%
        mutate(value = ifelse(is.na(value), 0, value),
               value_point = case_when( F1_0.2_0.8 == "Ind_right" ~ "black",
                                        F1_0.2_0.8 == "Ind_wrong" ~ "red2",
                                        T  ~ NA)
        ) %>%
        arrange(ingredient)
      
      ingr_levs <- temp$ingredient2
      temp$ingredient2 <- factor(temp$ingredient2, levels = ingr_levs)
      
      ingr_levs <- temp$ingredient3
      temp$ingredient3 <- factor(temp$ingredient3, levels = ingr_levs)
      
      p[[k]] <- 
        ggplot(temp, aes(x = ingredient3, y = value, group = -1)) +
        labs(x = "Ingredient", y = j) +
        scale_y_continuous( limits = c(point_level, 1), breaks = seq(0, 10, 2)/10, labels = scales::percent_format(scale = 100) ) + 
        scale_x_discrete() +
        geom_line() +
        geom_point(data = temp[ !is.na(temp$value_point), ], aes(x = ingredient3, y = point_level), col = temp$value_point[ !is.na(temp$value_point) ], shape = 18, size = 2 ) + 
        geom_hline(yintercept = 0.2, lty = "dashed", color = "red4", size = 0.5) +
        geom_hline(yintercept = 0.8, lty = "dashed", color = "green4", size = 0.5) +
        theme_minimal(base_size = 16) + 
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(face = "bold.italic", size = 8),
              axis.text.x = element_markdown(angle = 90, size = 5, hjust = 1, vjust = 0.5),
              legend.position = "none"
        )
      
    }
    
    p <- plot_grid(plotlist = p, nrow = 3, byrow = T)
    
    title_plot_y <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0., y = 0.5, label = s, size = 5, hjust = 0.5, vjust = 0.5, angle = 270)
    
    q[[r]] <- plot_grid(plotlist = list(p, title_plot_y), ncol = 2, rel_widths = c(1, 0.03))
    
  }
  
  q <- plot_grid(plotlist = q, nrow = 2, scale = 0.95)
  
  title_plot_y <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0.5, label = "F1 metrics", size = 7, hjust = 0.5, vjust = 0.5, angle = 90)
  
  q <- plot_grid(plotlist = list(title_plot_y, q), ncol = 2, rel_widths = c(0.03, 1))
  
  title_plot_x <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "Ingredient", size = 7, hjust = 0.5, vjust = 0.5)
  
  q <- plot_grid(plotlist = list(q, title_plot_x), nrow = 2, rel_heights = c(1, 0.03))
  
  title_plot <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = data_names_sorted[t], size = 7, hjust = 0.5, vjust = 0.5)
  
  q <- plot_grid(plotlist = list(title_plot, q), nrow = 2, rel_heights = c(0.04, 1))
  
  legend <-
    get_legend(
      ggplot(data_ingr_performance_list[(data_ingr_performance_list$model == m) & (data_ingr_performance_list$dataset == m) & (data_ingr_performance_list$scenario == s), ], aes(x = ingredient2, y = value, color = model, group = -1)) +
        labs(x = "Ingredient", y = "F1 score") +
        geom_line() +
        geom_hline(yintercept = 0.2, lty = "dashed", color = "red4", size = 0.5) +
        geom_hline(yintercept = 0.8, lty = "dashed", color = "green4", size = 0.5) +
        guides(color = guide_legend(title = "Algorithm", title.position = "left", nrow = 1, byrow = TRUE)) +
        scale_color_brewer(palette = "Spectral")
    )
  
  q
  
  if(doouts == 1){
    
    cat("...printing figure S", 12 + t, "of F1 score of ingredients...\n")
    
    par( mar = c(0, 0, 0, 0) )
    
    ggsave2(
      filename = paste0("./figures/figure_s", 12 + t, "-ingr_performances.pdf"),
      plot = q,
      width = 24, height = 17,
      scale = 2,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
}


p <- 
  ggplot(data_ingr_performance_list, aes(x = ingredient2, y = value, group = 1)) +
  labs(x = "Ingredient", y = "F1 score") +
  geom_line() +
  geom_hline(yintercept = cutoff_low, lty = "dashed", color = "red4", size = 0.5) +
  geom_hline(yintercept = cutoff_up, lty = "dashed", color = "green4", size = 0.5) +
  facet_grid(dataset ~ scenario, scale = "free_x" ) + 
  scale_fill_identity()+
  theme(axis.text.x = element_markdown(angle = 90, size = 4, hjust = 0, vjust = 0.5),
        legend.position = "none"
  )

p


if(doouts == 9999){
  
  cat("...printing alternative plot for F1 score ")
  
  par( mar = c(0, 0, 0, 0) )
  
  ggsave2(
    filename = paste0("./figures/plot-7-ingr_performances.pdf"),
    plot = p,
    width = 30, height = 20,
    scale = 1.5,
    units = c("cm"),
    dpi = 300,
    limitsize = TRUE
  )
  
  dev.off()
  
}


# table of aggregate F1 scores
tabella <- 
  data_ingr_performance_list %>%
  group_by(dataset, model, scenario) %>%
  filter(F1_0.2_0.8 %in% c("<0.2", "0.2<=x<=0.8", ">0.8" )) %>%
  summarise_at("value",     .funs = list(Min = ~ min(., na.rm = T),
                                         Q1 = ~ quantile(., 0.25, na.rm = T),
                                         Me = ~ median(., na.rm = T),
                                         Q3 = ~ quantile(., 0.75, na.rm = T),
                                         Max = ~ max(., na.rm = T),
                                         Mean = ~ mean(., na.rm = T),
                                         SD = ~ sd(., na.rm = T)
  )
  ) %>% 
  pivot_longer(everything(), names_sep = ':', names_to = c('.value', 'statistics')) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  regulartable() %>% autofit()
tabella

tabella_f1_num <- tabella


# table of categorical F1 scores
tabella <-
  data_ingr_performance_list %>%
  select(ingredient:F1_0.2_0.8, scenario, delta, ingredient3)  %>%
  filter(F1_0.2_0.8 %in% c("<0.2", "0.2<=x<=0.8", ">0.8" )) %>%
  group_by(across(c(
    model,
    dataset,
    scenario,
    F1_0.2_0.8
  ))) %>%
  count() %>%
  group_by(across(c(
    model,
    dataset,
    scenario
  ))) %>%
  mutate(n_tot = sum(n),
         n_pct = 100 * n/ n_tot
  ) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>% 
  pivot_longer(everything(), names_sep = ':', names_to = c('.value', 'statistics')) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  regulartable() %>% autofit()
tabella

tabella_f1_cat <- tabella

if(doouts == 1){
  cat("...printing table 4 (sliced) of aggregate ingredient performance...")
  save_as_docx(tabella_f1_cat, path = "./tables/table_4_f1-cat.docx")
  save_as_docx(tabella_f1_num, path = "./tables/table_4_f1-num.docx")
}


## delta F1 calculation:

# table of improvement from delta F1 (categorical)
tabella <-
  data_ingr_performance_list %>%
  filter(scenario == "After frame filtering" & delta_cat != "REAL") %>%
  select(ingredient:model, delta_cat, ingredient3)  %>%
  mutate(delta_cat = as.factor(case_when(str_detect(delta_cat, "Const") ~ "No changes", T ~ delta_cat))) %>%
  group_by(across(c(model, dataset, delta_cat)), .drop = FALSE) %>%
  count() %>%
  group_by(across(c(
    model,
    dataset
  ))) %>%
  mutate(n_pct = 100 * n / length(unique(data_ingr_performance_list$ingredient)),
         across(where(is.numeric), .fns =~ round(., 2)) 
  ) %>% 
  pivot_longer(everything(), names_sep = ':', names_to = c('.value', 'statistics')) %>%
  regulartable() %>% autofit()
tabella

tabella_delta_f1_cat <- tabella


tabella <- 
  data_ingr_performance_list %>%
  group_by(dataset, model) %>%
  filter( delta_cat == "REAL") %>%
  summarise(across(delta,
                   .names =  "{.col}:{.fn}", 
                   .fns = list(Min = ~ min(., na.rm = T),
                               Q1 = ~ quantile(., 0.25, na.rm = T),
                               Me = ~ median(., na.rm = T),
                               Q3 = ~ quantile(., 0.75, na.rm = T),
                               Max = ~ max(., na.rm = T),
                               Mean = ~ mean(., na.rm = T),
                               SD = ~ sd(., na.rm = T),
                               n = ~ n(),
                               n_prc =  ~ 100 * n() / length(unique(data_ingr_performance_list$ingredient))
                   )
  )
  ) %>% 
  pivot_longer(everything(), names_sep = ':', names_to = c('.value', 'statistics')) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  regulartable() %>% autofit()
tabella

tabella_delta_f1_num <- tabella


if(doouts == 1){
  cat("...printing table S3 (sliced) of deltas of ingredient performance...")
  save_as_docx(tabella_delta_f1_cat, path = "./tables/table_3_delta-f1-cat.docx")
  save_as_docx(tabella_delta_f1_num, path = "./tables/table_3_delta-f1-num.docx")
}


# plot of Delta F1
dish_ingr_performances_all_data_F1 <- 
  data_ingr_performance_list %>%
  filter((delta != "Inf") & !is.na(delta) ) %>%
  group_by(dataset) %>%
  arrange(desc(delta), .by_group = T) %>% 
  as.data.frame()


{
  
  q <- list()
  r <- 0
  
  for (m in levels(data_ingr_performance_list$dataset)) {
    
    p <- list()
    r <- r + 1
    k <- 0
    
    for(j in levels(data_ingr_performance_list$model)){
      
      k <- k + 1
      
      temp <-
        dish_ingr_performances_all_data_F1[(dish_ingr_performances_all_data_F1$dataset %in% m) &
                                             dish_ingr_performances_all_data_F1$model %in% j, ] %>%
        arrange(ingredient)
      
      ingr_levs <- temp$ingredient
      temp$ingredient <- factor(temp$ingredient, levels = ingr_levs)
      
      p[[k]] <- 
        ggplot(data = temp, mapping = aes(x = ingredient, y = delta, group = 1)) +
        labs(x = "Ingredient", y = j ) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), n.breaks = 5 ) + 
        theme_minimal(base_size = 12) + 
        geom_line() +
        geom_hline(yintercept = 0, lty = "dashed", color = "red3", size = 0.5) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(face = "bold.italic", size = 5),
              axis.text.x = element_text(angle = 90, size = 5, hjust = 0, vjust = 0.5),
              legend.position = "none"
        )
      
    }
    
    q[[r]] <- plot_grid(plotlist = p, nrow = 3, byrow = T)
    
    title_plot_y <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0., y = 0.5, label = m, size = 5, hjust = 0.5, vjust = 0.5, angle = 270)
    
    q[[r]] <- plot_grid(plotlist = list(q[[r]], title_plot_y), ncol = 2, rel_widths = c(1, 0.025))
    
  }
  
  q <- plot_grid(plotlist = q, nrow = 2, scale = 0.95)
  
  title_plot_y <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0.5, label = "Δ F1-score", size = 6, hjust = 0.5, vjust = 0.5, angle = 90)
  
  q <- plot_grid(plotlist = list(title_plot_y, q), ncol = 2, rel_widths = c(0.03, 1))
  
  title_plot_x <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "Ingredient", size = 6, hjust = 0.5, vjust = 0.5)
  
  q <- plot_grid(plotlist = list(q, title_plot_x), nrow = 2, rel_heights = c(1, 0.03))
  
  q
  
}

if(doouts == 9999){
  
  cat("...printing support figure of Delta F1 score of all ingredients...\n")
  
  par( mar = c(0, 0, 0, 0) )
  
  ggsave2(
    filename = paste0("./figures/plot-8-ingr_performances.pdf"),
    plot = q,
    width = 20, height = 14,
    scale = 2,
    units = c("cm"),
    dpi = 300,
    limitsize = TRUE
  )
  
  dev.off()
  
}


##### 4c - MISPREDICTED INGREDIENTS ######

if(doouts == 1){
  
  setwd( paste(wd_main, wd_output_data, sep = "/") )
  dir.create( paste(wd_main, wd_output_data, "mispredictions_dish_ingredient", sep = "/") )
  
  setwd( paste(wd_main, wd_output_data, "mispredictions_dish_ingredient", sep = "/") )
  
  dir.create("./figures/")
  dir.create("./support_data/")
  dir.create("./tables/") 
}


temp <-
  conf_matrix_list %>%
  select(ingredient:q3_dish_size) %>%
  mutate(F1 = case_when(F1_0.2_0.8 %in% c("Ind_right", "Ind_wrong", "<0.2") ~ NA,
                        T ~ F1)) %>%
  group_by(ingredient, dataset, scenario) %>%
  mutate_at(.vars = "F1", 
            .funs = list(#Min = ~min(., na.rm = T),
              Q1 = ~quantile(., 0.25, na.rm = T),
              Me = ~median(., na.rm = T),
              Q3 = ~quantile(., 0.75, na.rm = T),
              n_0.2 = ~ sum( F1_0.2_0.8 == "<0.2"),
              n_right = ~ sum( F1_0.2_0.8 == "Ind_right"),
              n_wrong = ~ sum( F1_0.2_0.8 == "Ind_wrong")
            )) %>%
  filter(n_0.2 == 3 | n_wrong == 3) %>%
  ungroup() %>%
  group_by(scenario)

list_mispred_ingredients <- temp


temp <-
  list_mispred_ingredients %>%
  filter(scenario == "After frame filtering")

target_ingredients <- unique(temp$ingredient)

table_mispred_ingredients <-
  list_mispred_ingredients %>%
  filter(scenario == "After frame filtering" & ingredient %in% target_ingredients) %>%
  arrange(ingredient, dataset, model)


if(doouts == 1){
  cat("...printing data for table S7 of  figure of  misclassified ingredients after frame filtering...\n")
  write.xlsx(x = table_mispred_ingredients, file = "./tables/table_s7_misclass_ingr_after.xlsx")
}


##### 4d - cooccurrences #####

temp <-
  df_fin_reportino3_ingr_list %>%
  filter(dataset == "US FCBD—correction" & 
           model == "IncV3_2+2" & 
           scenario == "Before frame filtering"
  ) %>%
  select(recipe_id, almonds_gt:zucchini_gt) %>% # observed values
  mutate(across( ends_with("_gt"), .fns = ~ as.numeric(.) - 1 )) %>%
  rename_all(.funs = ~gsub(., pattern = "_gt", replacement = ""))

temp <-
  df_nutrition5k %>%
  filter(recipe_id %in% training_dishes) %>%
  select(recipe_id, contains(gsub(x = ingredients_training_set, pattern = "_pred", replacement = "")))

temp2 <- matrix(0, nrow = ncol(temp) - 1, ncol = ncol(temp) - 1)
colnames(temp2) <- colnames(temp)[-1]
row.names(temp2) <- colnames(temp)[-1]

for(i in 1:nrow(temp2)){
  for(j in 1:nrow(temp2)){
    temp2[i, j] <- crossprod(x = temp[, i + 1], y = temp[, j + 1])
  }
}

coocc_obs <- as.data.frame(temp2)

n_obs = dim(temp)[1]
occ_diag <- diag(temp2)
occ_total_sum <- rowSums(temp2) - occ_diag
order_ingr_to_show <- gsub(firstup(names(sort(occ_diag, decreasing = T))), pattern = "_", replacement = " " ) 

top_match <- 10 # top 10 matches
n_ingr_to_show <- 20 #top 20 ingredient

temp <- as.data.frame( matrix( 0, ncol = 3 + top_match, nrow = ncol(temp2) ) )
colnames(temp) <- c("ingredient", "n_occ", "n_tot", paste("n", 1:top_match, sep = "_"))

temp[, 1] <- row.names(temp2)
temp[, 2] <- occ_diag
temp[, 3] <- occ_total_sum

temp_ingr <- as.data.frame( matrix( 0, ncol = 1 + top_match, nrow = ncol(temp2) ) )
colnames(temp_ingr) <- c("ingredient", paste("ingr", 1:top_match, sep = "_"))
temp_ingr[, 1] <- row.names(temp2)


for(i in 1:nrow(temp)){
  
  temp[i, 3 + c(1:top_match)] <- sort(temp2[i, -c(i)], decreasing = T)[1:top_match]
  temp_ingr[i, -c(1)] <- names(sort(temp2[i, -c(i)], decreasing = T)[1:top_match])
  
}

ingr_occ_order <- gsub(firstup(temp[order(temp$n_occ, decreasing = T), "ingredient"]), pattern = "_", replacement = " " ) 

temp <-
  temp %>%
  merge(temp_ingr, by = "ingredient" ) %>%
  mutate(across(contains("ingr"),  ~gsub(., pattern = "_", replacement = " "))) %>%
  pivot_longer(!"ingredient", names_sep = '_', names_to = c('.value', 'rank')) %>%
  mutate(ingredient = firstup(ingredient)) %>%
  mutate(across(!n, as.factor))  %>%
  na.omit() %>%
  rowwise() %>%
  mutate(ingr = paste(n, "-", ingr),
         rank = factor(rank, levels = c(1:top_match, "occ", "tot"))
  ) %>%
  filter( ingredient %in% ingr_occ_order[1:n_ingr_to_show]) %>%
  mutate(ingredient = factor(ingredient, levels = order_ingr_to_show[1:n_ingr_to_show], labels = paste0(order_ingr_to_show[1:n_ingr_to_show], " (n = ", as.numeric(sort(occ_diag, decr = T))[1:n_ingr_to_show], ")")))


p <-
  ggplot(temp, aes(x = ingredient , y = rank, label = ingr)) + 
  geom_point(stat='identity', fill="black", size = 6, shape = 15)  +
  labs(x = paste("First", n_ingr_to_show, "ingredients (total count), n = ", n_obs), y = "Rank") +
  scale_x_discrete(limits = rev) +
  geom_segment(aes(y = 0, 
                   x = ingredient, 
                   yend = rank, 
                   xend = ingredient
  ), 
  color = "black",
  lty = "dashed",
  size = 0.3,
  alpha = 0.1
  ) +
  geom_label(color = "black", size = 4) +
  theme_minimal(base_size = 16) + 
  coord_flip()

p

if(doouts == 9999){
  
  cat("...printing support figures for cooccurrences...\n")
  
  par( mar = c(0, 0, 0, 0) )
  
  ggsave2(
    filename = paste0("./support_data/rank_top_", n_ingr_to_show, "_", n_obs, "_ingr_cooccurrences.pdf"),
    plot = p,
    width = 20, height = 14,
    scale = 2.6,
    units = c("cm"),
    dpi = 300,
    limitsize = TRUE
  )
  
  dev.off()
  
}


for(k in levels(df_fin_reportino3_ingr_list$scenario)){
  
  for(r in levels(df_fin_reportino3_ingr_list$dataset)){
    
    for(m in levels(df_fin_reportino3_ingr_list$model)){
      
      temp <-
        df_fin_reportino3_ingr_list %>%
        filter(dataset == r & 
                 model == m & 
                 scenario == k
        ) %>%
        select(recipe_id, almonds_gt:zucchini_gt) %>%
        mutate(across( ends_with("_gt"), .fns = ~ as.numeric(.) - 1 )) %>%
        rename_all(.funs = ~gsub(., pattern = "_gt", replacement = ""))
      
      temp2 <- matrix(0, nrow = ncol(temp) - 1, ncol = ncol(temp) - 1)
      colnames(temp2) <- colnames(temp)[-1]
      row.names(temp2) <- colnames(temp)[-1]
      temp2 <- as.data.frame(temp2)
      
      for(i in 1:nrow(temp2)){
        for(j in 1:nrow(temp2)){
          temp2[i, j] <- crossprod(x = temp[, i + 1], y = temp[, j + 1])
        }
      }
      
      if(doouts == 9999){
        
        write.xlsx(x = temp2, file = paste0(paste("./support_data/ingr_cooccurrences", substr(i, start = 1, 6), substr(r, start = 1, 2), substr(m, start = 1, 4), sep = "_"), ".xlsx"))
        
      }
      
      test <- vegan::mantel(xdis = temp2[row.names(temp2) != "plate_only", (which(names(temp2) != "plate_only"))], ydis = coocc_obs[row.names(coocc_obs) %in% names(temp2)[ (which(names(temp2) != "plate_only")) ], names(coocc_obs) %in% names(temp2)], method = "pearson", permutations = 1000 )
      
      cat("testing within: ", k, " in ", r, " for ", m, "...p-value from MH test = ",  test$signif, "\n")
      
    }
    
  }
  
}


#### 5 - ANALYSIS of NUTRITIONAL COMPOSITION ####

###### 5a - nutritional values single prediction ######

if(doouts == 1){
  
  setwd( paste(wd_main, wd_output_data, sep = "/") )
  wd = paste(wd_main, wd_output_data, "nutritional_value_prediction", "comparisons", sep = "/")
  dir.create( wd, recursive = T)
  setwd( wd )
  
  dir.create("./figures/")
  dir.create("./support_data/")
  dir.create("./tables/")
}

# nutrients predictions by datasets and scenarios
df_fin_reportino3_list <- 
  df_fin_reportino3_list %>% 
  bind_rows()  %>%
  select( c(recipe_id, model, dataset, scenario), contains(substr(x = nutrients, start = 1, stop = 4) ) ) %>%
  filter( endsWith( model,("2+2") ) & grepl(x = dataset, pattern = "corr", fixed = F) ) %>%
  mutate(dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted),
         scenario = factor(scenario, levels = names(list_ff_order), labels = as.character(list_ff_order))
  )


q <- list()

for(k in 1:5){
  
  temp <- df_fin_reportino3_list %>% select( c("model", "dataset", "scenario") | contains(substr(x = nutrients_sorted[k], start = 1, stop = 4) ))
  names(temp)[c(4, 5)] <- c("y", "x")
  
  temp$y <- temp$y - temp$x
  
  temp_median <- temp %>%
    group_by(dataset, scenario) %>%
    summarise_at(
      .vars = "x",
      .names =  "{.col}:{.fn}",
      .funs = list(
        q1 = ~ quantile(., 0.25, na.rm = T),
        me = ~ median(., na.rm = T),
        q3 = ~ quantile(., 0.75, na.rm = T)
      )
    )
  
  temp_segments <- temp %>%
    summarise_at(.vars = "y",
                 .funs = list(
                   min = ~ 1.05 * min(.),
                   max = ~ 1.05 * max(.)
                 )
    )
  
  temp_values <- cbind(temp_median, min = (1/1.05) * temp_segments$min)
  
  y = nutrients_names_sorted[k]
  
  q <-   
    ggplot(data = temp) +
    geom_jitter(data = temp, mapping = aes(x = x, y = y, col = model)) +
    geom_hline(yintercept = 0, col = "red3", lty = "dashed", size = .8) + 
    geom_segment(data = temp_values, aes(x = q1, y = min, yend = Inf), col = "red3", lty = "dashed", size = .4) + 
    geom_segment(data = temp_values, aes(x = me, y = min, yend = Inf), col = "red3", lty = "dashed", size = .4) + 
    geom_segment(data = temp_values, aes(x = q3, y = min, yend = Inf), col = "red3", lty = "dashed", size = .4) + 
    geom_smooth(method = "gam", 
                mapping = aes(x = x, y = y, col = model),
                size = 0.4) + 
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(n.breaks = 10, limits = c(temp_segments$min[1], temp_segments$max[1] )) + 
    facet_grid(dataset ~ scenario, axes = "all", scales = "free_x") +
    geom_text(data = temp_median, aes(x = q1, y = -Inf, 
                                      label = paste(round(q1))), 
              vjust = -0.5, color = "red", size = 4) +
    geom_text(data = temp_median, aes(x = me, y = -Inf, 
                                      label = paste(round(me))), 
              vjust = -0.5, color = "red", size = 4) +
    geom_text(data = temp_median, aes(x = q3, y = -Inf, 
                                      label = paste(round(q3))), 
              vjust = -0.5, color = "red", size = 4) +
    labs(title = y, x = "Observed values", y = "Predicted - observed values", col = "Algorithm") +
    guides(fill = guide_legend(title = "Algorithm", title.position = "left", nrow = 1, byrow = TRUE)) +
    theme_minimal(base_size = 18) +
    theme( plot.title = element_text(hjust = .5),
           strip.text.y = element_text(size = 12),
           legend.position = "bottom" ) +
    scale_color_manual(values = c("orange", "steelblue", "green3"))
  
  q
  
  if(doouts == 1){
    
    #par(mar = c(0, 0, 0, 0))
    
    ggsave2(
      filename = paste0("./figures/supfig-s", k, "_", nutrients_sorted[k], "-scenarios.pdf"),
      plot = q,
      width = 16, height = 10,
      scale = 2,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
}


###### 5b - performances between publications ######

# dataset of nutritional composition of single dishes from Bianco et al. 2025
df_fin_reportino2_before <- 
  clean_names(read.csv(file = paste(wd_main, wd_input_data, "reportino2_rerun_3alg/data/df_fin_reportino2.csv", sep = "/"),
                       sep = ";", dec = ","
                       )
              ) %>%
  select(recipe_id:mass_gt) %>%
  filter( str_detect(string = dataset, pattern = 'corr') & 
          str_detect(string = model, pattern = '2\\+2') & model != "R50_IN1k_2+2"
  ) %>%
  mutate( dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted),
          model = gsub(x = model, pattern = "_IN1k", replacement = "")
  ) %>%
  mutate(id = paste( recipe_id , dataset, model, sep = "_"),
         scenario = "Reportino2_before"
  ) %>%
  relocate(id, scenario, .before = recipe_id)

# dataset of nutritional values in the current publication before frame filtering
df_fin_reportino3_before <-
  df_fin_reportino3_list %>%
  select(recipe_id:mass_gt) %>%
  filter( scenario == "Before frame filtering" &
          str_detect(string = dataset, pattern = 'corr') & 
          str_detect(string = model, pattern = '2\\+2') & model != "R50_IN1k_2+2"
  ) %>%
  mutate(id = paste( recipe_id , dataset, model, sep = "_"),
         scenario = "Reportino3_before"
  ) %>%
  relocate(id, scenario, .before = recipe_id)


#dataset containing only predicted and observed values of single dishes over the two publications (vs B25)
df_fin_reportino2.3_before_single_values <- merge( x = df_fin_reportino2_before, df_fin_reportino3_before, by = "id", suffixes = c("_r2", "_r3"))


## calculating MAPE contribution of single dishes:

# retrieving data of single performances from the current analysis
df_fin_reportino3_before <- 
  dato_5runs_performances_list %>%
  bind_rows() %>%
  filter( scenario == names(which(list_ff_order == "Before frame filtering")) &
          str_detect(string = dataset, pattern = 'corr') & 
          str_detect(string = model, pattern = '2\\+2') & model != "R50_IN1k_2+2"
  ) %>%
  mutate(dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted),
         run = as.factor(run)
  ) %>%
  group_by(recipe_id, dataset, model, run) %>%
  mutate(across(any_of(cols_res),
                list(RMSE = ~RMSE(.),
                     MAE = ~MAE(.)),
                .names = "{.fn}_{.col}"),
         across(any_of(cols_abs),
                list(MAPE = MAPE), .names = "{.fn}_{.col}")
  ) %>% 
  ungroup() %>%
  group_by(recipe_id, dataset, model) %>%
  select(c(recipe_id, model, dataset, contains("MAPE"))) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(scenario = "Reportino3_before") %>%
  relocate(scenario, .after = model)

#  retrieving data of single performances from Biano et al. 2025
df_fin_reportino2_before <- 
  read.xlsx(xlsxFile = paste(wd_main, wd_input_data, "reportino2_rerun_3alg/data/dishes_single_performances.xlsx", sep = "/") ) %>%
  select(recipe_id, dataset, model, contains( c("pred_res", "pred_abs") )) %>%
  filter( str_detect(string = dataset, pattern = 'corr') & 
          str_detect(string = model, pattern = '2\\+2') & model != "R50_IN1k_2+2"
  ) %>%
  mutate( dataset = factor(dataset, levels = datsets_sorted, labels = data_names_sorted),
          model = gsub(x = model, pattern = "_IN1k", replacement = "")
  ) %>%
  select(c(recipe_id, model, dataset, contains("MAPE"))) %>%
  mutate(scenario = "Reportino2_before") %>%
  relocate(names(df_fin_reportino3_before))

# all data
df_fin_reportino2.3_before <- 
  bind_rows(df_fin_reportino2_before,
            df_fin_reportino3_before
  ) %>%
  relocate(scenario, .after = dataset)

names(df_fin_reportino2.3_before)


# creating residuals of MAPEs: res_MAPE = MAPE_reportino3 - MAPE_reportino2
for(i in 1:5){
  
  nomi_temp <- paste0("res_", substr(x = names(df_fin_reportino2.3_before)[4 + i], start = 1, stop = 8))
  cat(nomi_temp, "\n")
  
  df_fin_reportino2.3_before[, ncol(df_fin_reportino2.3_before) + 1] <- NA
  names(df_fin_reportino2.3_before)[ncol(df_fin_reportino2.3_before) ] <- nomi_temp
  
  for(j in unique(df_fin_reportino2.3_before$recipe_id)){
    
    for(k in names(table(df_fin_reportino2.3_before$model))){
      
      for(r in names(table(df_fin_reportino2.3_before$dataset))){
        
        righe <- df_fin_reportino2.3_before$recipe_id %in% j & 
          df_fin_reportino2.3_before$model %in% k &
          df_fin_reportino2.3_before$dataset %in% r 
        
        df_fin_reportino2.3_before[righe & (df_fin_reportino2.3_before$scenario %in% "Reportino3_before"), ncol(df_fin_reportino2.3_before)] <- 
          df_fin_reportino2.3_before[ righe & (df_fin_reportino2.3_before$scenario %in% "Reportino3_before"), 4 + i] -  
          df_fin_reportino2.3_before[ righe & (df_fin_reportino2.3_before$scenario %in% "Reportino2_before"), 4 + i]
        
      }
      
    }
    
  }
  
}

tabella <-  
  df_fin_reportino2.3_before %>%
  filter(scenario == "Reportino3_before") %>%
  select(recipe_id:model, contains("res"), -scenario) %>%
  rename_at(vars(contains("res_MAPE")), .funs = ~ gsub(., pattern = "res_MAPE_", replacement = "")) %>%
  pivot_longer(!(recipe_id:model), names_to = 'nutrient', values_to = "value") %>%
  group_by(dataset, model, nutrient) %>%
  summarise_at(.vars = "value", 
                   .funs = list(Min = ~ min(., na.rm = T),
                               Q1 = ~ quantile(., 0.25, na.rm = T),
                               Me = ~ median(., na.rm = T),
                               Q3 = ~ quantile(., 0.75, na.rm = T),
                               Max = ~ max(., na.rm = T)
                               #Mean = ~ mean(., na.rm = T),
                               #SD = ~ sd(., na.rm = T)
                   )
  ) %>% 
  mutate(nutrient = factor(nutrient, levels = substr(start = 1, stop = 3, x = nutrients_sorted), labels = nutrients_names_sorted)) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  arrange(dataset, model, nutrient) %>%
  regulartable() %>% autofit()
tabella

table_MAPE_diff <- tabella  

if(doouts == 1){
  cat("...printing table S10 of Delta MAPE distribution...\n")
  save_as_docx(table_MAPE_diff, path = paste0("./tables/table_s10_deltaMAPE.docx"))  
}


###### 5ba - regressions for MAPEs between publications ######

df_fin_reportino2.3_before_reg <-
  df_fin_reportino2.3_before %>%
  filter(scenario == "Reportino3_before") %>%
  select(recipe_id:model, contains("res_MAPE") & !scenario) %>%
  merge(y = 
          df_nutrition5k %>%
          select(recipe_id:recipes) %>%
          mutate(raw_cooked = as.factor(cut(raw_cooked,
                                            breaks = c(0, 1, 2, max(raw_cooked)),
                                            include.lowest = T, right = F, labels = c("No diff", "One", "More")
          )
          ),
          recipes =  as.factor(cut(recipes,
                                   breaks = c(0, 1, max(recipes)),
                                   include.lowest = T, right = F, labels = c(" No", " Yes")
          )
          ),
          total_mass = as.numeric(gsub(x = total_mass, pattern = ",", replacement = "."))/25
          ),
        all.x = T, by = "recipe_id"
  ) %>%
  mutate(across(contains("res_MAPE"), ~ abs(.x)),
         num_ingr_per_dish = as.factor(cut(num_ingr_per_dish, breaks = c(0, 1, 3, 9, max(num_ingr_per_dish, na.rm = T)))),
         model = factor(model, levels = c("R101_2+2", "ViT-B-16_2+2", "IncV3_2+2"))
  )


i <- 1
tabella <- list()

regressors <- "~ dataset + model + num_ingr_per_dish #+ total_mass + (raw_cooked * recipes)"

varnames <- 
  list(
    "dataset" = "FCDB",
    "model" = "Algorithm",
    "total_mass" = "Total mass (x25g)",
    "num_ingr_per_dish" = "Number of ingredients",
    "recipes" = "Retreated recipes in dish",
    "raw_cooked" = "Differencial use of raw/cooked ingredients"
  )

for(k in paste("res_MAPE", substr(nutrients_sorted, start = 1, stop = 3), sep = "_")){
  
  i <- i + 1
  cat("Studying:", k, "\n")
  
  regressors2 <- regressors
  
  if(str_detect(string = k, pattern = "mas")){
    regressors2 <- gsub(x = regressors, pattern = "total_mass +", replacement = "")
  }
  
  formula <- as.formula(paste(k, regressors2))
  
  set.seed(1234); mod <- lmRob(formula = formula, data = df_fin_reportino2.3_before_reg)
  anova_test <- as.data.frame(anova(mod, test = "RF")) %>% mutate_if(is.numeric, ~ round(., digits = 3))
  cat("ANOVA test: \n")
  print(anova_test)
  
  set.seed(1234); mod <- MASS::rlm(formula = formula, data = df_fin_reportino2.3_before_reg, method = "MM", steps = 200)
  summaries <- summary(mod, method = "XtWX")
  
  j <- 0
  rob_pval <- NA 
  for(s in names(mod$coefficient)) {
    
    #cat("method for", toupper(s), ":  ", p, "\n")
    j <- j + 1
    
    p <- round(as.numeric(f.robftest(mod, var = s)$p), 3)
    rob_pval[j] <- p 
  }
  
  # 95% CIs
  cat("Coefficients from LRM: \n")
  lower <- as.numeric(coef(summaries)[,1]) - pnorm(q = 1 - p/2, mean = 0, sd = 1)*as.numeric(coef(summaries)[,2]); 
  upper <- as.numeric(coef(summaries)[,1]) + pnorm(q = 1 - p/2, mean = 0, sd = 1)*as.numeric(coef(summaries)[,2]); cbind(round(coef(mod), 2), round(lower, 2),round(upper, 2))
  temp <- data.frame(coef = row.names(coef(summaries)), low95 = lower, est =  as.numeric(coef(summaries)[, 1]), upp95 = upper, p = rob_pval)
  print(temp)
  
  tabella[[i]] <- 
    tbl_regression(x = mod,
                   intercept = F, label = varnames,
                   pvalue_fun = ~style_pvalue(.x, digits = 3),
                   estimate_fun = partial(style_ratio, digits = 2),
    )  %>%
    add_global_p() %>%
    #bold_p(t = 0.05) %>%
    bold_labels() %>%
    italicize_levels()
  tabella[[i]] 
  
  
  cat("############## ...be careful to p-values ## \n\n")
  
  for(j in unique(tabella[[i]] $table_body$variable)){ 
    
    if(j %in% names(df_fin_reportino2.3_before_reg) && is.factor(df_fin_reportino2.3_before_reg[[j]])){
      
      temp_spec <- emmeans::emmeans(object = mod, specs = j)
      temp_df <- as.data.frame(temp_spec)
      
      rows_to_update <- which(tabella[[i]] $table_body$variable %in% j & tabella[[i]] $table_body$row_type == "level")
      
      tabella[[i]] $table_body[rows_to_update, "estimate"]  <- temp_df$emmean #betas
      tabella[[i]] $table_body[rows_to_update, "ci"]  <- paste(round(temp_df$asymp.LCL, 2), round(temp_df$asymp.UCL, 2), sep = ", ") #CIs
      tabella[[i]] $table_body[rows_to_update, "p.value"]   <- 2 * pnorm(abs(temp_df$emmean / temp_df$SE), lower.tail = FALSE) # pvalues
    }
    
  }
  
}

tabella[[1]] <- 
  tbl_regression(x = mod,
                 exponentiate = F, intercept = T, label = varnames, 
                 conf.int = F,
                 pvalue_fun = ~style_pvalue(.x, digits = 3),
                 estimate_fun = partial(style_ratio, digits = 2),
  )  %>%
  add_n(location = "level") %>%
  bold_labels() %>%
  italicize_levels()

tabella_rlm <- tbl_merge(tabella, tab_spanner = c("N", nutrients_names_sorted)) %>% as_flex_table

if(doouts == 9999){
  cat("...printing support table of models...BE CAREFUL WITH p-VALUEs WHICH ARE NOT SUPPORTED BY THE ADOPTED PACKAGEs...\n")
  save_as_docx(tabella_rlm, path = "./tables/table_RLM.docx")
}


##### 5bb - plot of MAPEs between publications #####
temp <- 
  df_fin_reportino2.3_before %>%
  select(recipe_id:model, contains("pred_abs")) %>%
  rename_at(vars(contains("MAPE_")), .funs = ~ gsub(., pattern = "MAPE_", replacement = "")) %>%
  pivot_longer(!(recipe_id:model), names_to = 'nutrient', names_pattern = "(.*)_pred_abs$",
               values_to = "value"
  ) %>%
  group_by(dataset, model, nutrient, scenario) %>%
  summarise(value = mean(value, na.rm = T)) %>%  #mean value is to obtain MAPE!
  mutate(nutrient = factor(nutrient, 
                           levels = gsub(x = nutrients_sorted, pattern = "_gt", replacement = ""), 
                           labels = nutrients_names_sorted)
  ) %>%
  mutate(id = paste(nutrient, dataset, model, scenario, sep = "_"),
         scenario = factor(scenario, levels = paste0("Reportino", 2:3, "_before"), labels = c("Bianco et al., 2025", "Current analysis"))
  )


temp1 <-
  df_fin_reportino2.3_before %>%
  select(recipe_id:model, contains("res_")) %>%
  rename_at(vars(contains("res_MAPE")), .funs = ~ gsub(., pattern = "res_MAPE_", replacement = "")) %>%
  pivot_longer(!(recipe_id:model), names_to = 'nutrient',
               values_to = "value"
  ) %>%
  group_by(dataset, model, nutrient) %>%
  summarise(value = mean(value, na.rm = T)) %>% #mean value is to obtain MAPE!
  mutate(scenario = "Current analysis",
         nutrient = factor(nutrient, 
                           levels = substr( x = nutrients_sorted, start = 1, stop = 3), 
                           labels = nutrients_names_sorted),
  ) %>%
  mutate(id = paste(nutrient, dataset, model, "Reportino3_before", sep = "_")) %>%
  rename("diff" = "value")


temp <- merge(x = temp, y = temp1, all.x = T)

# plot of percent differences in MAPE between the two pubblication, by dataset and algorithm
{
  
  "MAPE"
  k <- 0
  p <- list()
  lim_y <- max(temp$value) # fix limit for axis
  
  for (m in levels(temp$dataset)) {
    
    k <- k + 1
    labelling = element_text(size = 10)
    labelling2 = element_blank()
    if(k > 1){
      labelling = element_blank()
      labelling2 = element_text(size = 10)
    }
    
    p[[k]] <- 
      ggplot(temp[(temp$dataset == m), ], aes(x = model, y = value, fill = model)) +
      labs(x = "Algorithm", y = "MAPE", fill = "Algorithm") +
      geom_col(position = position_dodge(0.55)) +
      scale_x_discrete(limits = rev, drop = F) +
      coord_flip() +
      facet_grid(nutrient ~ scenario) +
      geom_text(data = temp[(temp$dataset == m) & (temp$scenario == "Current analysis"), ],
                aes( x = model, y = value, label = format(x = round(x = diff, digits = 2), nsmall = 2) ), color = "black", fontface = "bold.italic", size = 4, hjust = 1.1) +
      scale_y_continuous(n.breaks = 5, limits = c(0, lim_y), labels = scales::percent_format(scale = 1)) +
      theme_minimal(base_size = 16) +
      theme(strip.text.x = element_text(size = 14),
            strip.text.y = labelling2,
            axis.text.y = labelling,
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none") +
      scale_fill_manual(values = c("orange", "steelblue", "green3"))
    
    title_plot <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = data_names_sorted[k], size = 6, hjust = 0.5, vjust = 0.5)
    
    p[[k]] <- plot_grid(plotlist = list(title_plot, p[[k]]), nrow = 2, rel_heights = c(0.05, 1))
    
  }
  
  p <- plot_grid(plotlist = p, ncol = 2, rel_widths = c(1.1, 1))
  
  title_plot_x <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.6, label = "MAPE", size = 7, hjust = 0.5, vjust = 0.5)
  
  p <- plot_grid(plotlist = list(p, title_plot_x), nrow = 2, rel_heights = c(1, 0.03))
  
  title_plot_y <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0.5, label = "Algorithm", size = 7, hjust = 0.5, vjust = 0.5, angle = 90)
  
  p <- plot_grid(plotlist = list(title_plot_y, p), ncol = 2, rel_widths = c(0.03, 1))
  
  p
  
}


if(doouts == 9999){
  
  cat("...printing support figure of MAPE difference between the current pubblication vs Bianco et al. 2025...\n")
  
  par( mar = c(0, 0, 0, 0) )
  
  ggsave2(
    filename = paste0("./figures/supfig_X_mape-rep_2_3_before.pdf"),
    plot = p,
    width = 10, height = 6.5,
    scale = 4,
    units = c("cm"),
    dpi = 300,
    limitsize = TRUE
  )
  
  dev.off()
  
}


# wilcoxon rank-sum test for residuals difference in reportino3 vs reportino2
table_wilk_diff <-
  df_fin_reportino2.3_before %>%
  filter(scenario == "Reportino3_before") %>%
  group_by(model, dataset) %>%
  summarise(across(contains("res"), ~ wilcox.test(.)$p.value, .names = "{.col}")) %>%
  mutate_if(is.numeric, ~ round(., 3))  %>%
  regulartable() %>% autofit() 

if(doouts == 9999){
  cat("...printing support table of p-values from Mann-Whitney test on MAPE difference between the current pubblication vs Bianco et al., 2025...\n")
  save_as_docx(table_wilk_diff, path = paste0("./tables/table2_reportino2.3-wilcoxon.docx"))
}


# support file: tables of difference of MAPE residuals reportino3 vs reportino2, before frame filtering by more groups

# you can focus on a subset of dishes by filtering by 'target dishes'
target_dishes <- c("dish_1558630325", "dish_1558720236", "dish_1562703447", "dish_1563389626", "dish_1566838351",
                   "dish_1563551194", "dish_1566316757", "dish_1566849987", "dish_1568146942", "dish_1563898084",
                   "dish_1563898084", "dish_1563551194")

table_target_dishes <-
  df_fin_reportino2.3_before %>%
  #filter( recipe_id %in% target_dishes ) %>%
  group_by(dataset, scenario, model) %>%
  select( 
    #contains("res_MAPE"), # choose the variables to calculate statistics (residuals of MAPE rep3 vs rep2)
    contains("pred"),    # choose the variables to calculate statistics (MAPEs of rep3, rep2) 
  ) %>%
  rename_all(.funs = ~gsub(., pattern = "MAPE_|_pred_abs", replacement = "")) %>%
  #rename_all(.funs = ~gsub(., pattern = "res_MAPE_", replacement = "")) %>% filter(scenario == "Reportino3_before") %>%
  rename_all(.funs = ~gsub(., pattern = ":", replacement = "_")) %>%
  summarise_all(
    .funs = list(#Min = ~ min(., na.rm = T),
      Q1 = ~ quantile(., 0.25, na.rm = T),
      Me = ~ median(., na.rm = T),
      Q3 = ~ quantile(., 0.75, na.rm = T)
      #Max = ~ max(., na.rm = T)
      #Mean = ~ mean(., na.rm = T)
      #SD = ~ sd(., na.rm = T)
    )
  ) %>% 
  pivot_longer(!c(dataset, model, scenario), names_sep = '_', names_to = c('.value', 'statistics')) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  regulartable() %>% autofit()

if(doouts == 9999){
  cat("...printing support table of MAPE difference between the current pubblication vs Bianco et al., 2025 for a subset of dishes...\n")
  save_as_docx(table_target_dishes, path = paste0("./support_data/tableX_reportino2_3_before.docx"))  
}


##### 5bc - kernel densities #####

{
  
  temp <-
    df_fin_reportino2.3_before %>%
    filter( scenario == "Reportino3_before" ) %>%
    select(c(recipe_id, dataset, model, contains("res_MAPE"))) %>%
    rename_with(~ gsub(., pattern = "res_MAPE_", replacement = ""), any_of(contains("res_MAPE"))) %>%
    relocate(recipe_id, dataset, model, mas, cal, fat, pro, car) %>%
    as.data.frame
  
  n_eps <- 6  
  n_dec <- 1
  custom_cols = c("red2","green4", "steelblue2","grey", "grey4")
  
  p <- list()
  q <- list()
  
  for(j in 1:5){
    
    minval = min(temp[,j + 3])
    maxval = max(temp[,j + 3])
    
    temp_median <- temp %>%
      summarise_at(.vars = vars(names(temp)[j + 3]), 
                   .funs = list(me =~ median(.),
                                q1 =~ quantile(., 0.25),
                                q3 =~ quantile(., 0.75)
                   )
      )
    
    breaks = seq(minval, maxval, length.out = n_eps)
    breaks = breaks[- which.min(abs( breaks )) ]
    breaks = as.numeric(sort(c(0, breaks)))
    labels = as.character(round(breaks, n_dec))
    
    dens_vals <- density(temp[[names(temp)[j + 3]]])
    y_max <- max(dens_vals$y * length(temp[[1]]))
    
    p[[j]] <-
      ggplot(data = temp, aes_string(x = names(temp)[j + 3])) +
      geom_density(aes(y = after_stat(count)),
                   fill = custom_cols[j],
                   alpha = 0.3) +
      #geom_point(data = temp_median, mapping = aes(x = q1, y = 0), col = custom_cols[j], size = 1.5, shape = 17) + 
      geom_point(data = temp_median, mapping = aes(x = me, y = 0), col = custom_cols[j], size = 0.8, shape = 8) + 
      #geom_point(data = temp_median, mapping = aes(x = q3, y = 0), col = custom_cols[j], size = 1.5, shape = 13) + 
      geom_vline(xintercept = 0, col = "red2", lty = "dashed", size = 0.5) + 
      scale_x_continuous(limits = c(minval, maxval), breaks = breaks, labels = labels) + 
      scale_y_continuous(n.breaks = n_eps, expand = c(0.025, 0)) +
      coord_cartesian(ylim = c(0, y_max)) +
      labs(x = nutrients_names_sorted[j], y = NULL) +
      theme_minimal(base_size = 5)
    
    
  }
  
  q <- plot_grid(plotlist = p, nrow = 1); q
  
  
  if(doouts == 1){
    
    cat("...printing figure S16 of kernel density...\n")
    
    ggsave2(
      filename = paste0("./figures/figure_s16_kernel_overall.pdf"),
      plot = q,
      width = 8, height = 5,
      scale = 2,
      units = c("cm"),
      dpi = 800,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
  
  # by dataset
  
  p <- list()
  q <- list()
  
  for(j in 1:5){
    
    temp_median <- temp %>%
      select(c(dataset, names(temp)[j + 3])) %>%
      group_by(dataset) %>%
      summarise_at(.vars = vars(names(temp)[j + 3]), 
                   .funs = list(me =~ median(.),
                                q1 =~ quantile(., 0.25),
                                q3 =~ quantile(., 0.75)
                   )
      )
    
    minval = min(temp[,j + 3])
    maxval = max(temp[,j + 3])
    
    dens_vals <- density(temp[[names(temp)[j + 3]]])
    y_max <- max(dens_vals$y * length(temp[[1]]))
    
    breaks = seq(minval, maxval, length.out = n_eps)
    breaks = breaks[- which.min(abs( breaks )) ]
    breaks = as.numeric(sort(c(0, breaks)))
    labels = as.character(round(breaks, n_dec))
    
    p[[j]] <-
      ggplot(data = temp, aes_string(x = names(temp)[j + 3], col = "dataset")) +
      geom_density(aes(y = after_stat(count)), 
                   #size = 1.3
                   alpha = 0.2
      ) +
      #geom_point(data = temp_median, mapping = aes(x = q1, y = 0, col = dataset), size = 1.5, shape = 17) + 
      geom_point(data = temp_median, mapping = aes(x = me, y = 0, col = dataset), size = 1.5, shape = 8) + 
      #geom_point(data = temp_median, mapping = aes(x = q3, y = 0, col = dataset), size = 1.5, shape = 13) + 
      geom_vline(xintercept = 0, col = "red2", lty = "dashed", size = 0.5) + 
      scale_x_continuous(limits = c(minval, maxval), breaks = breaks, labels = labels) + 
      scale_y_continuous(n.breaks = n_eps,  expand = c(0.025, 0)) + 
      coord_cartesian(ylim = c(0, y_max)) +
      labs(x = nutrients_names_sorted[j], y = NULL) +
      theme_minimal(base_size = 5) +
      theme(axis.title.y = element_blank(),
            legend.position = "none"
      ) +
      scale_color_manual(values = c("orange", "green3"))
    
    legend <-
      get_legend(
        ggplot(data = temp, aes_string(x = names(temp)[j + 3], fill = "dataset")) +
          geom_density(aes(y = after_stat(count)), 
                       #size = 1.3
                       #alpha = 0.2
          ) +
          scale_fill_manual(values = c("orange", "green3")) +
          guides(fill = guide_legend(title = "Dataset", title.position = NULL, nrow = 1, byrow = TRUE)) +
          theme_minimal(base_size = 5) +
          theme(legend.title = element_text(size = 8),
                legend.position = "bottom",
                legend.key.size = unit(0.5, 'cm'), #change legend key size
                legend.key.height = unit(0.5, 'cm'), #change legend key height
                legend.key.width = unit(0.5, 'cm'), #change legend key width
                legend.text = element_text(size = 6) #
          )
      )
    
  }
  
  p <- plot_grid(plotlist = p, nrow = 1)
  
  q <- plot_grid(plotlist = list(p, legend), nrow = 2, rel_heights = c(1, 0.05))  + theme(plot.margin = margin(0, 8, 0, 0)); q
  
  if(doouts == 1){
    
    cat("...printing figure S17 of kernel density...\n")
    
    ggsave2(
      filename = paste0("./figures/figure_s17_kernel_dataset.pdf"),
      plot = q,
      width = 8, height = 5,
      scale = 2.,
      units = c("cm"),
      dpi = 800,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
  # by algorithm
  p <- list()
  q <- list()
  
  for(j in 1:5){
    
    temp_median <- temp %>%
      select(c(model, names(temp)[j + 3])) %>%
      group_by(model) %>%
      summarise_at(.vars = vars(names(temp)[j + 3]), 
                   .funs = list(me =~ median(.),
                                q1 =~ quantile(., 0.25),
                                q3 =~ quantile(., 0.75)
                   )
      )
    
    minval = min(temp[,j + 3])
    maxval = max(temp[,j + 3])
    
    dens_vals <- density(temp[[names(temp)[j + 3]]])
    y_max <- max(dens_vals$y * length(temp[[1]]))
    
    breaks = seq(minval, maxval, length.out = n_eps)
    breaks = breaks[- which.min(abs( breaks )) ]
    breaks = as.numeric(sort(c(0, breaks)))
    labels = as.character(round(breaks, n_dec))
    
    p[[j]] <-
      ggplot(data = temp, aes_string(x = names(temp)[j + 3], col = "model")) +
      geom_density(aes(y = after_stat(count)), 
                   #size = 1.3
                   alpha = 0.2
      ) +
      #geom_point(data = temp_median, mapping = aes(x = q1, y = 0, col = model), size = 1.5, shape = 17) + 
      geom_point(data = temp_median, mapping = aes(x = me, y = 0, col = model), size = 0.8, shape = 8) + 
      #geom_point(data = temp_median, mapping = aes(x = q3, y = 0, col = model), size = 1.5, shape = 13) + 
      geom_vline(xintercept = 0, col = "red2", lty = "dashed", size = 0.5) + 
      scale_x_continuous(limits = c(minval, maxval), breaks = breaks, labels = labels) + 
      scale_y_continuous(n.breaks = n_eps, expand = c(0.025, 0)) +
      labs(x = nutrients_names_sorted[j], y = NULL) +
      theme_minimal(base_size = 5) +
      theme(axis.title.y = element_blank(),
            legend.position = "none"
      ) +
      scale_color_manual(values = c("orange", "steelblue", "green3"))
    
    legend <-
      get_legend(
        ggplot(data = temp, aes_string(x = names(temp)[j + 3], fill = "model")) +
          geom_density(aes(y = after_stat(count)), 
                       #size = 1.3
                       #alpha = 0.2
          ) +
          scale_fill_manual(values = c("orange", "steelblue", "green3")) +
          guides(fill = guide_legend(title = "Algorithm", title.position = NULL, nrow = 1, byrow = TRUE)) +
          theme_minimal(base_size = 5) +
          theme(legend.title = element_text(size = 8),
                legend.position = "bottom",
                legend.key.size = unit(0.5, 'cm'), #change legend key size
                legend.key.height = unit(0.5, 'cm'), #change legend key height
                legend.key.width = unit(0.5, 'cm'), #change legend key width
                legend.text = element_text(size = 6) #
          ) 
      )
    
  }
  
  p <- plot_grid(plotlist = p, nrow = 1); p
  
  q <- plot_grid(plotlist = list(p, legend), nrow = 2, rel_heights = c(1, 0.05))  + theme(plot.margin = margin(0, 8, 0, 0)); q
  
  if(doouts == 1){
    
    cat("...printing figure S18 of kernel density...\n")
    
    ggsave2(
      filename = paste0("./figures/figure_s18_kernel_model.pdf"),
      plot = q,
      width = 8, height = 5,
      scale = 2.,
      units = c("cm"),
      dpi = 800,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
  
  # by dataset & algorithm
  q <- list()
  
  for(k in 1:2){
    
    p <- list()
    
    for(j in 1:5){
      
      temp_median <- temp %>%
        filter(dataset == levels(dataset)[k]) %>%
        select(c(model, names(temp)[j + 3])) %>%
        group_by(model) %>%
        summarise_at(.vars = vars(names(temp)[j + 3]), 
                     .funs = list(me =~ median(.),
                                  q1 =~ quantile(., 0.25),
                                  q3 =~ quantile(., 0.75)
                     )
        )
      
      minval = min(temp[,j + 3])
      maxval = max(temp[,j + 3])
      
      breaks = seq(minval, maxval, length.out = n_eps)
      breaks = breaks[- which.min(abs( breaks )) ]
      breaks = as.numeric(sort(c(0, breaks)))
      labels = as.character(round(breaks, n_dec))
      
      p[[j]] <-
        ggplot(data = temp, aes_string(x = names(temp)[j + 3], col = "model")) +
        geom_density(aes(y = after_stat(count)), 
                     #size = 1.3
                     alpha = 0.2
        ) +
        #geom_point(data = temp_median, mapping = aes(x = q1, y = 0, col = model), size = 1.5, shape = 17) + 
        geom_point(data = temp_median, mapping = aes(x = me, y = 0, col = model), size = 0.8, shape = 8) + 
        #geom_point(data = temp_median, mapping = aes(x = q3, y = 0, col = model), size = 1.5, shape = 13) + 
        geom_vline(xintercept = 0, col = "red2", lty = "dashed", size = 0.5) + 
        scale_x_continuous(limits = c(minval, maxval), breaks = breaks, labels = labels) + 
        scale_y_continuous(n.breaks = n_eps,  expand = c(0.025, 0)) + 
        labs(x = nutrients_names_sorted[j]) +
        theme_minimal(base_size = 5) +
        theme(axis.title.y = element_blank(),
              legend.position = "none") +
        scale_color_manual(values = c("orange", "steelblue", "green3"))
      
      legend <-
        get_legend(
          ggplot(data = temp, aes_string(x = names(temp)[j + 3], fill = "model")) +
            geom_density(aes(y = after_stat(count)), 
                         #size = 1.3
                         #alpha = 0.2
            ) +
            scale_fill_manual(values = c("orange", "steelblue", "green3")) +
            guides(fill = guide_legend(title = "Algorithm", title.position = NULL, nrow = 1, byrow = TRUE)) +
            theme_minimal(base_size = 5) +
            theme(legend.title = element_text(size = 8),
                  legend.position = "bottom",
                  legend.key.size = unit(0.5, 'cm'), #change legend key size
                  legend.key.height = unit(0.5, 'cm'), #change legend key height
                  legend.key.width = unit(0.5, 'cm'), #change legend key width
                  legend.text = element_text(size = 6) #
            ) 
        )
      
    }
    
    p <- plot_grid(plotlist = p, nrow = 1)
    
    title_plot_y <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0., y = 0.5, label = rev(data_names)[k], size = 4, hjust = 0.5, vjust = 0.5, angle = 90)
    
    q[[k]] <-
      plot_grid(plotlist = list(title_plot_y, p), ncol = 2, rel_widths = c(0.03, 1))
    
  }
  
  q <-  plot_grid(plotlist = q, nrow = 2)
  
  q <-  plot_grid(plotlist = list(q, legend), nrow = 2, rel_heights = c(1, 0.05))  + theme(plot.margin = margin(0, 0, 0, 0)); q
  
  if(doouts == 1){
    
    cat("...printing figure 5 of kernel density...\n")
    
    ggsave2(
      filename = paste0("./figures/figure_5_kernel_dataset_algorithm.pdf"),
      plot = q,
      width = 8, height = 5,
      scale = 2.,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
}


##### 5c - MAPE comparison in the current analysis between frame filtering scenario, and by dataset #####

# dishes performance  by datasets and scenarios
dato_5runs_perf_aggr_all_data <-
  dato_5runs_perf_aggr_all_data %>%
  bind_rows() %>%
  filter(dataset %in% data_names_sorted) %>% 
  group_by(nutrient, scenario, model, metric) %>%
  mutate(dataset = factor(dataset, levels = data_names_sorted),
         scenario = factor(scenario, levels = names(list_ff_order), labels = as.character(list_ff_order)),
         model = factor(model, levels = model, labels = gsub(x = model, pattern = "IN1k_", replacement = "")),
         delta = NA
  ) %>%
  mutate(dataset_scenario = as.factor(paste(dataset, scenario, sep = " \n "))
  ) %>%
  mutate(dataset_scenario = factor(dataset_scenario, levels = as.vector(t(outer(data_names_sorted, list_ff_order, paste, sep = " \n "))))) %>%
  ungroup() %>%
  group_by(nutrient, scenario, metric, dataset) %>%
  mutate(point_level = median(value)) %>%
  ungroup() %>% distinct() %>% as.data.frame()


# plots of performance over scenarios

{
  
  i <- 0
  q <- list()
  
  for(k in 1:nlevels(dato_5runs_perf_aggr_all_data$metric)){
    
    title_metric <- levels(dato_5runs_perf_aggr_all_data$metric)[k]             
    
    for(s in 1:nlevels(dato_5runs_perf_aggr_all_data$nutrient)){
      
      i <- i + 1
      labelling = NULL
      str_dir = NULL
      
      title_target <- levels(dato_5runs_perf_aggr_all_data$nutrient)[s]
      
      temp <- as.data.frame(subset(dato_5runs_perf_aggr_all_data, (dato_5runs_perf_aggr_all_data$metric == title_metric) & (dato_5runs_perf_aggr_all_data$nutrient == title_target))) 
      
      
      lim2 <- 1.0 * max(temp$value)
      
      if( i <= 5){ 
        title_target_set = element_text(size = 9)
        title_data_set = element_text(size = 8)
      }else{
        title_target_set = element_blank()
        title_data_set = element_blank()
      }
      
      if( (i-1) %% 5 == 0){ 
        title_metric_set = element_text(size = 10, hjust = 0.5, vjust = -2, face = "bold.italic")
      }else{
        title_metric_set = element_blank() 
      }
      
      if(k == 3){
        labelling = scales::percent_format(scale = 1) 
      }else{
        labelling = scales::number_format()
      }
      
      temp_segments <- temp %>%
        distinct(dataset_scenario, point_level) %>%
        mutate(
          x = as.numeric(rev(dataset_scenario)),
          xstart = x - 0.3,
          xend   = x + 0.3
        )
      
      q[[i]] <- 
        ggplot(temp, aes(x = dataset_scenario, y = value)) +
        geom_col(aes(fill = model), width = 0.8, position = position_dodge(0.8)) +
        labs(title = title_metric, x = title_target) +
        scale_x_discrete(limits = rev, drop = F) +
        coord_flip() +
        geom_point(data = temp_segments,
                   aes(x = dataset_scenario, y = point_level)
        ) +
        geom_segment(data = temp_segments,
                     aes(x = xstart, xend = xend, y = point_level, yend = point_level),
                     color = "black", size = 0.7
        ) +
        geom_line(aes(x = dataset_scenario, y = point_level, group = pair), color = "red3", lty = "dotted", size = 0.8) +
        theme_minimal(base_size = 20) +
        scale_y_continuous(n.breaks = 7, labels = labelling) +
        theme(axis.title.y = title_target_set,
              axis.text.y = title_data_set,
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 8, vjust = 10),
              plot.title = title_metric_set,
              plot.margin = rep(unit(0, "null"), 4),
              panel.margin = unit(0, "null"),
              legend.position = "none") +
        scale_fill_manual(values = c("orange", "steelblue", "green3"))
      
    }
    
  }
  
  legend <-
    get_legend(
      ggplot(temp, aes(x = dataset_scenario, y = value, fill = model)) +
        geom_col(position = position_dodge(0.55)) +
        guides(fill = guide_legend(title = "Algorithm", title.position = "left", nrow = 1, byrow = TRUE)) +
        scale_fill_manual(values = c("orange", "steelblue", "green3"))
    )
  
  p <- plot_grid(plotlist = q, nrow = 5, byrow = F, rel_widths = c(1.3, 1, 1))
  
  title_plot_y <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0.5, label = "Nutritional variable, dataset, and frame filtering", size = 7, hjust = 0.5, vjust = .5, angle = 90)
  
  p <- plot_grid(plotlist = list(title_plot_y, p), ncol = 2, rel_widths = c(0.05, 1))
  
  p <- 
    plot_grid(plotlist = list(p, legend), nrow = 2, rel_heights = c(1, 0.03))  + theme(plot.margin = margin(0, 8, 0, 0))
  
  p
  
}

if(doouts == 1){
  
  cat("...printing figure S15 of algorithms performance metrics over scenarios and dataset...\n")
  
  par(mar = c(0, 0, 0, 0))
  
  ggsave2(
    filename = paste0("./figures/figure_s15_performances-scenarios.pdf"),
    plot = p,
    width = 14, height = 20,
    scale = 2.,
    units = c("cm"),
    dpi = 1200,
    limitsize = TRUE
  )
  
  dev.off()
  
}


# dishes performances delta (%) over scenarios

for(l in levels(dato_5runs_perf_aggr_all_data$nutrient)) {
  
  for (m in levels(dato_5runs_perf_aggr_all_data$dataset)) {
    
    for (r in levels(dato_5runs_perf_aggr_all_data$model)) {
      
      for (s in levels(dato_5runs_perf_aggr_all_data$metric)) {
        
        righe <- (dato_5runs_perf_aggr_all_data$nutrient %in% l) & (dato_5runs_perf_aggr_all_data$dataset %in% m) & (dato_5runs_perf_aggr_all_data$model %in% r) & (dato_5runs_perf_aggr_all_data$metric %in% s)
        righe2 <- righe & dato_5runs_perf_aggr_all_data$scenario %in% levels(dato_5runs_perf_aggr_all_data$scenario)[2]
        
        dato_5runs_perf_aggr_all_data[righe2, "delta"] <-
          100 * (dato_5runs_perf_aggr_all_data[righe2 , "value"] - dato_5runs_perf_aggr_all_data[righe & dato_5runs_perf_aggr_all_data$scenario %in% levels(dato_5runs_perf_aggr_all_data$scenario)[1], "value"]) /
          dato_5runs_perf_aggr_all_data[righe & dato_5runs_perf_aggr_all_data$scenario %in% levels(dato_5runs_perf_aggr_all_data$scenario)[1], "value"]
        
      }
      
    }
    
  }
  
}


# plotting MAPE for the current publication by frame dataset and filtering scenario

{
  
  metric_name = "MAPE" # change to plot other metrics
  k <- 0
  p <- list()
  
  for (m in levels(dato_5runs_perf_aggr_all_data$dataset)) {
    
    k <- k + 1
    labelling = element_text(size = 16)
    labelling2 = element_blank()
    if(k > 1){
      labelling = element_blank()
      labelling2 = element_text(size = 16)
    }
    
    temp <- 
      dato_5runs_perf_aggr_all_data %>%
      filter( (dataset == m) & (metric == metric_name) ) %>%
      mutate(nutrient = factor(nutrient, labels = gsub(x = levels(nutrient), pattern = "content", replacement = "\ncontent" )))
    
    p[[k]] <- 
      ggplot(temp, aes(x = model, y = value, fill = model)) +
      labs(x = "Algorithm", y = metric_name, fill = "Algorithm") +
      geom_col(position = position_dodge(0.55)) +
      scale_x_discrete(limits = rev, drop = F) +
      coord_flip() +
      facet_grid(nutrient ~ scenario) +
      geom_text(data = temp[ temp$scenario == "After frame filtering", ],
                aes(x = model, y = value, label = paste0( format(x = round(x = delta, digits = 2), nsmall = 2), "%" )), color = "black", fontface = "bold.italic", size = 5, hjust = 1.1) +
      scale_y_continuous(n.breaks = 5, limits = c(0, 50), labels = scales::percent_format(scale = 1)) +
      theme_minimal(base_size = 16) +
      theme(strip.text.x = element_text(size = 20),
            strip.text.y = labelling2,
            axis.text.y = labelling,
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none") +
      scale_fill_manual(values = c("orange", "steelblue", "green3"))
    
    title_plot <-   
      ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = data_names_sorted[k], size = 6, hjust = 0.5, vjust = 0.5)
    
    p[[k]] <- plot_grid(plotlist = list(title_plot, p[[k]]), nrow = 2, rel_heights = c(0.05, 1))
    
  }
  
  p <- plot_grid(plotlist = p, ncol = 2, rel_widths = c(1.1, 1))
  
  title_plot_x <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.6, label = "MAPE", size = 7, hjust = 0.5, vjust = 0.5)
  
  p <- plot_grid(plotlist = list(p, title_plot_x), nrow = 2, rel_heights = c(1, 0.03))
  
  title_plot_y <-   
    ggplot() +
    theme_void() +
    annotate("text", x = 0., y = 0.5, label = "Algorithm", size = 7, hjust = 0.5, vjust = 0.5, angle = 90)
  
  p <- plot_grid(plotlist = list(title_plot_y, p), ncol = 2, rel_widths = c(0.03, 1))
  
  show(p)
  
  
  if(doouts == 1){
    
    cat("printing figure 3 of MAPE percent difference after vs before frame filtering...\n")
    
    par(mar = c(0, 0, 0, 0))
    
    ggsave2(
      filename = paste0("./figures/figure_3_mape-scenario.pdf"),
      plot = p,
      width = 10, height = 6.5,
      scale = 4,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
    
    dev.off()
    
  }
  
}


# table of MAPE for the current publication by frame dataset and filtering scenario
tabella <-
  dato_5runs_perf_aggr_all_data %>%
  filter( metric == "MAPE") %>%
  group_by(dataset, # choose grouping variables
           scenario, 
           #model
  ) %>%
  summarise(across(c(delta, # choose MAPE [value] or delta MAPE
                     value
  ),
  .names =  "{.col}:{.fn}", 
  .fns = list(Min = ~ min(., na.rm = T),
              Q1 = ~ quantile(., 0.25, na.rm = T),
              Me = ~ median(., na.rm = T),
              Q3 = ~ quantile(., 0.75, na.rm = T),
              Max = ~ max(., na.rm = T),
              Mean = ~ mean(., na.rm = T),
              SD = ~ sd(., na.rm = T)
  )
  )
  ) %>% 
  pivot_longer(everything(), names_sep = ':', names_to = c('.value', 'statistics')) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  regulartable() %>% autofit()
tabella

tabella_MAPE_scenarios <- tabella

if(doouts == 9999){
  cat("...printing support table of MAPE and Delta MAPE distribution over scenarios...\n")
  save_as_docx(tabella_MAPE_scenarios, path = "table_MAPE_scenarios.docx")
}


###### 5d - MISPREDICTED DISHES analysis between publications and frame filtering scenario ######

if(doouts == 1){
  
  setwd( paste(wd_main, wd_output_data, sep = "/") )
  dir.create( paste(wd_main, wd_output_data, "mispredictions_dish_ingredient", sep = "/") )
  
  setwd( paste(wd_main, wd_output_data, "mispredictions_dish_ingredient", sep = "/") )

}

mispred_reportino3_list <-
  bind_rows(mispred_reportino3_list) %>%
  mutate(scenario = factor(scenario, levels = names(list_ff_order), labels = as.character(list_ff_order)))

mispred_reportino2 <- read.xlsx( xlsxFile = paste(wd_main, wd_input_data, "reportino2_rerun_3alg/data/dishes_mispredicted_reportino2_rerun.xlsx", sep = "/"), sheet = "reportino2_list_unique_dishes")

list_rep2_before_rerun <- mispred_reportino2$recipe_id

list_rep3_before <- unique(mispred_reportino3_list[mispred_reportino3_list$scenario == "Before frame filtering", "recipe_id"])

list_rep3_after <- unique(mispred_reportino3_list[mispred_reportino3_list$scenario == "After frame filtering", "recipe_id"])

'
# you will find these dishes
list_rep3_before <- c(
  "dish_1550873668", "dish_1557937079", "dish_1558372948", "dish_1558373159", "dish_1558375667", "dish_1558375886", "dish_1558376984", "dish_1558544663",
  "dish_1558549806", "dish_1558630325", "dish_1558720236", "dish_1560367952", "dish_1560367980", "dish_1560368506", "dish_1560368570", "dish_1560800988", 
  "dish_1560801020", "dish_1560801041", "dish_1561577848", "dish_1561577947", "dish_1561661377", "dish_1561999624", "dish_1561999706", "dish_1562009934",
  "dish_1562618000", "dish_1562691064", "dish_1562703447", "dish_1562790824", "dish_1562790855", "dish_1563305513", "dish_1563389600", "dish_1563389626",
  "dish_1563478751", "dish_1563551194", "dish_1563551220", "dish_1563566909", "dish_1563566939", "dish_1563566965", "dish_1563898084", "dish_1563909550",
  "dish_1563909580", "dish_1563984199", "dish_1563984242", "dish_1563984296", "dish_1563998323", "dish_1565030391", "dish_1565123881", "dish_1565811061",
  "dish_1565811139", "dish_1565898230", "dish_1565974409", "dish_1565986761", "dish_1566246626", "dish_1566316726", "dish_1566328776", "dish_1566328805",
  "dish_1566328831", "dish_1566414342", "dish_1566414412", "dish_1566501575", "dish_1566501594", "dish_1566502573", "dish_1566587182", "dish_1566590007",
  "dish_1566590056", "dish_1566838351", "dish_1566838378", "dish_1566838407", "dish_1566850031", "dish_1568147009", "dish_1568147044", "dish_1568305257",
  "dish_1568401302"
)

list_rep2_before_rerun <- c(  # see complete file "list_mispredicted_reportino3_3alg")
  "dish_1550873668", "dish_1557937079", "dish_1558372948", "dish_1558373159", "dish_1558375886", "dish_1558544663", "dish_1558549806", "dish_1558630325",
  "dish_1558720236", "dish_1560203424", "dish_1560367952", "dish_1560367980", "dish_1560368506", "dish_1560368570", "dish_1560800988", "dish_1560801020",
  "dish_1560801041", "dish_1561577848", "dish_1561577947", "dish_1561661377", "dish_1561999624", "dish_1561999706", "dish_1562009934", "dish_1562618000",
  "dish_1562691064", "dish_1562790855", "dish_1563305513", "dish_1563389600", "dish_1563389626", "dish_1563478751", "dish_1563551220", "dish_1563566909",
  "dish_1563566939", "dish_1563566965", "dish_1563909550", "dish_1563909580", "dish_1563984140", "dish_1563984242", "dish_1563984296", "dish_1565030391",
  "dish_1565123881", "dish_1565811061", "dish_1565811139", "dish_1565894910", "dish_1565898230", "dish_1565974409", "dish_1565986761", "dish_1566246513",
  "dish_1566246626", "dish_1566316681", "dish_1566316726", "dish_1566328724", "dish_1566328776", "dish_1566328805", "dish_1566328831", "dish_1566414342",
  "dish_1566414412", "dish_1566501575", "dish_1566501594", "dish_1566502573", "dish_1566587182", "dish_1566590007", "dish_1566590056", "dish_1566838351",
  "dish_1566838378", "dish_1566838407", "dish_1566850031", "dish_1568147009", "dish_1568147044", "dish_1568305257", "dish_1568401302"
)

list_rep3_after <- c(
  "dish_1550873668", "dish_1557937079", "dish_1558372948", "dish_1558373159", "dish_1558375886", "dish_1558544663", "dish_1558630325", "dish_1558720236",
  "dish_1560367952", "dish_1560367980", "dish_1560368506", "dish_1560368570", "dish_1560800988", "dish_1560801020", "dish_1560801041", "dish_1561577848",
  "dish_1561577947", "dish_1561661377", "dish_1561999624", "dish_1561999706", "dish_1562009934", "dish_1562618000", "dish_1562691064", "dish_1562703447",
  "dish_1562790855", "dish_1563305513", "dish_1563389600", "dish_1563389626", "dish_1563478751", "dish_1563551194", "dish_1563551220", "dish_1563566909",
  "dish_1563566939", "dish_1563566965", "dish_1563898084", "dish_1563909550", "dish_1563909580", "dish_1563984296", "dish_1565030391", "dish_1565123881",
  "dish_1565811061", "dish_1565811139", "dish_1565898230", "dish_1565974409", "dish_1566246626", "dish_1566316757", "dish_1566328776", "dish_1566328805",
  "dish_1566328831", "dish_1566414342", "dish_1566414412", "dish_1566501575", "dish_1566501594", "dish_1566502573", "dish_1566587182", "dish_1566590007",
  "dish_1566590056", "dish_1566838351", "dish_1566838378", "dish_1566838407", "dish_1566849987", "dish_1566850031", "dish_1568146942", "dish_1568147009",
  "dish_1568147044", "dish_1568305257", "dish_1568401302"
)

'

# R3before intersected intersection R3after (including problematic dishes in Bianco 2025)
intersection_before_after <- 
  intersect(x = list_rep3_after, y = list_rep3_before)

# R3before intersected with R3after (excluding problematic dishes in Bianco 2025)
intersection_before_after_noprobl <- 
  intersect(x = unique(mispred_reportino3_list[(mispred_reportino3_list$scenario == "Before frame filtering") & (mispred_reportino3_list$problematic_dish == 1), "recipe_id"]),
            y = unique(mispred_reportino3_list[(mispred_reportino3_list$scenario == "After frame filtering") & (mispred_reportino3_list$problematic_dish == 1), "recipe_id"]))

# realigned dishes in R3 after, e.g. R3before \ intersection(R3before, R3after)
realligned_after <- 
  unique(mispred_reportino3_list[(mispred_reportino3_list$scenario == "Before frame filtering") & !(mispred_reportino3_list$recipe_id %in% intersection_before_after), "recipe_id"])

# new entries dishes in R3 after, e.g. R3after \ intersection(R3before, R3after)
new_entries_after <- 
  unique(mispred_reportino3_list[(mispred_reportino3_list$scenario == "After frame filtering") & !(mispred_reportino3_list$recipe_id %in% intersection_before_after), "recipe_id"])

# R3before intersected with R2before rerun:
rep3_rep2_rerun_before <- 
  intersect(x = list_rep3_before, y = list_rep2_before_rerun)

# R3after \ R2before_rerun # 5 dishes
rep3_after_no_rep2_rerun_before <- list_rep3_after[(! (list_rep3_after %in% mispred_reportino2$recipe_id)) & (! list_rep3_after %in% intersection_before_after_noprobl)]

# R3before \ R2before_rerun # 8 dishes
rep3_no_rep2_rerun_before <- list_rep3_before[! (list_rep3_before %in% list_rep2_before_rerun) ]

# R2before_rerun \ R3before # 6 sishes
rep2_rerun_no_rep3_before <- list_rep2_before_rerun[! (list_rep2_before_rerun %in% list_rep3_before) ]

## you will find these dishes
'
rep3_after_no_rep2_rerun_before <- # 5 dishes in mispred rep3_after & not rep2_before
c("dish_1563551194", "dish_1563898084", "dish_1566316757", 
  "dish_1566849987", "dish_1568146942"
  )

rep2_rerun_no_rep3_before <-   # 6 dishes in mispred. rep2_before_rerun & not rep3_before
  c("dish_1560203424", "dish_1563984140", "dish_1565894910",
    "dish_1566246513", "dish_1566316681", "dish_1566328724"
  ) 

rep3_no_rep2_rerun_before <-  # 8 dishes in mispred. rep3_before & not rep2_before_rerun
  c("dish_1558375667", "dish_1558376984", "dish_1562703447", "dish_1562790824", 
    "dish_1563551194", "dish_1563898084", "dish_1563984199", "dish_1563998323" 
  )
  
'

# detect the statistics of ingredients within some dishes of interest
target_dishes = rep3_after_no_rep2_rerun_before

# creating list of dishes of interest to count detected ingredient and their relevation over algotirhms, across datasets
target_dishes_nutr <-
  conf_matrix_list[
    !(conf_matrix_list[, target_dishes] %in% c("TN")),] %>%
  select(c("dataset","model", "scenario", "ingredient", target_dishes)) %>% 
  pivot_longer(cols = contains("dish"), names_to = "recipe_id") %>%
  filter(value != "TN" & scenario == "After frame filtering") %>%
  merge( y = df_nutrition5k[, c("recipe_id", "num_ingr_per_dish") ]) %>%
  arrange(recipe_id, desc(dataset), model) %>%
  group_by(recipe_id, ingredient, num_ingr_per_dish, value) %>%
  summarise(count = n()) %>%
  mutate(flag_5_6 = ifelse(count > 4, 1, 0)) %>%
  ungroup() %>%
  group_by(recipe_id, value) %>%
  mutate(prevalence_detection = 100 * sum(flag_5_6)/num_ingr_per_dish,
         ingredient_list_by_cell = paste(ingredient, collapse = ", ")
  ) %>% 
  group_by(recipe_id) %>%
  mutate(ingredient = paste(unique(ingredient[value %in% c("FlsNeg", "TP")]), collapse = ", ")) %>% #TO SOLVE REPETITIONs
  unique() %>% 
  arrange(recipe_id, value)


# print observed and predicted values over reportinos  
target_list_name <- "5_dishes"
target_dishes <- rep3_after_no_rep2_rerun_before

target_dishes_ingr <- 
  conf_matrix_list %>%
  select(ingredient:scenario, target_dishes) %>%
  filter(scenario == "After frame filtering") %>%
  pivot_longer(cols = target_dishes, names_to = "recipe_id", values_to = "value") %>%
  filter(value != "TN") %>%
  group_by(recipe_id, ingredient, value) %>%
  summarise(conteggio = sum(value == "TP")) %>%
  group_by(recipe_id) %>%
  mutate(additional_ingrs = sum(value == "FlsPos"),
         value = factor(value, levels = c("TP", "FlsNeg", "FlsPos"))
         ) %>%
  group_by(recipe_id, ingredient) %>%
  filter(any(value == "TP")) %>%
  arrange(recipe_id, value, -conteggio,ingredient)
  

target_dishes_nutr <-
  df_fin_reportino3_list %>%
  filter(recipe_id %in% target_dishes & scenario == "After frame filtering") %>%
  select(!scenario) %>%
  group_by(recipe_id) %>%
  summarise(across(ends_with("pred")| ends_with("gt"),
                 .names =  "{.col}:{.fn}", 
                 .fns = list(#Q1 = ~ quantile(., 0.25, na.rm = T),
                             Me = ~ median(., na.rm = T)
                             #Q3 = ~ quantile(., 0.75, na.rm = T),
                             #Mean = ~ mean(., na.rm = T),
                             #SD = ~ sd(., na.rm = T)
                             )
                 )
            ) %>% 
  pivot_longer(cols = -recipe_id, names_sep = ':', names_to = c('.value', 'statistics')) %>%
  select(!statistics)

target_dishes_nutr_ingr <-
  target_dishes_ingr %>%
  full_join(target_dishes_nutr, by = "recipe_id")


if(doouts == 1){
  cat("...printing data for table S9 of 5 dishes - new mispredicted after frame filtering in the current analysis - ...\n")
  write.xlsx(x = target_dishes_nutr_ingr, file = paste0("./tables/table_s9_mispred-dishes_5_after_vs_R2_no-rerun.xlsx"))
}


# print observed and predicted values over reportinos  
target_list_name <- "6_and_8_dishes"
target_dishes_6 <- rep2_rerun_no_rep3_before
target_dishes_8 <- rep3_no_rep2_rerun_before

temp1 <- 
  conf_matrix_list %>%
  filter(scenario == "After frame filtering") %>%
  select(ingredient:model, c(target_dishes_6, target_dishes_8)) %>%
  pivot_longer(cols = c(target_dishes_6, target_dishes_8), names_to = "recipe_id", values_to = "value") %>%
  mutate(set = case_when(recipe_id %in% target_dishes_6 ~ "6_dishes",
                         recipe_id %in% target_dishes_8 ~ "8_dishes")
         ) %>%
  filter(!value %in% "TN") %>%
  group_by(recipe_id, ingredient, dataset, model, set) %>%
  mutate(n_TP = case_when(sum(value == "TP") == 1 ~ "Yes",
                          sum(value == "TP") == 0 ~ "No",
                          T ~ NA
                          )
         ) %>%
  group_by(recipe_id, dataset, model, set) %>%
  filter(ingredient != " ") %>%
  ungroup() %>%
  mutate(data_model = paste(dataset, model, sep = "_")) %>%
  pivot_wider(
    id_cols = everything(),
    names_from = data_model,
    values_from = n_TP,
    values_fill = "No"
  ) %>%
  relocate(recipe_id, ingredient, .before = dataset)
  
  
temp2 <- 
  conf_matrix_list %>%
  filter(scenario == "After frame filtering") %>%
  select(ingredient:model, c(target_dishes_6, target_dishes_8)) %>%
  pivot_longer(cols = c(target_dishes_6, target_dishes_8), names_to = "recipe_id", values_to = "value") %>%
  mutate(set = case_when(recipe_id %in% target_dishes_6 ~ "6_dishes",
                         recipe_id %in% target_dishes_8 ~ "8_dishes")
  ) %>%
  filter(value != "TN") %>%
  mutate(data_model = paste(dataset, model, sep = "_")) %>%
  group_by(recipe_id, dataset, model, set) %>%
  mutate(additional_ingrs = paste(ingredient[value == "FlsPos"], collapse = ", "),
         n_additional_ingrs = sum(value == "FlsPos")
  ) %>%
  group_by(recipe_id, data_model, set) %>%
  summarise(
    ingredient = "False positive",
    value = if(all(additional_ingrs == "None" | additional_ingrs == "")) "None" 
    else first(additional_ingrs),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(recipe_id, ingredient),
    names_from = data_model,
    values_from = value
  )

target_dishes_ingr <- bind_rows(x = temp1, y = temp2)

target_dishes_nutr <-
  df_fin_reportino3_list %>%
  filter(recipe_id %in% c(target_dishes_6, target_dishes_8) & scenario == "After frame filtering") %>%
  select(!scenario) %>%
  mutate(set = case_when(recipe_id %in% target_dishes_6 ~ "6_dishes",
                         recipe_id %in% target_dishes_8 ~ "8_dishes"
                         )
  ) %>%
  group_by(recipe_id, set) %>%
  summarise(across(ends_with("pred")| ends_with("gt"),
                   .names =  "{.col}:{.fn}", 
                   .fns = list(#Q1 = ~ quantile(., 0.25, na.rm = T),
                     Me = ~ median(., na.rm = T)
                     #Q3 = ~ quantile(., 0.75, na.rm = T),
                     #Mean = ~ mean(., na.rm = T),
                     #SD = ~ sd(., na.rm = T)
                   )
  )
  ) %>% 
  pivot_longer(cols = -c(recipe_id, set), names_sep = ':', names_to = c('.value', 'statistics')) %>%
  select(!statistics)

target_dishes_nutr_ingr <-
  target_dishes_ingr %>%
  left_join(target_dishes_nutr, by = c("recipe_id", "set"))  %>%
  arrange(recipe_id, ingredient, set)


if(doouts == 1){
  cat("printing data for tables S11 of 6 dishes - mispredicted in the rerun of Bianco et al. 2025 and not in this analysis before frame filtering - ...\n")
  cat("printing data for tables S12 of 8 dishes - mispredicted in this analysis before frame filtering and not in the rerun of Bianco et al. 2025 and not  - ...\n")
  write.xlsx(x = target_dishes_nutr_ingr, file = paste0("./tables/table_s11-s12_mispred-dishes_6-8_rerun_R2.xlsx"))
}


# support material: stastistics on target dishes
temp <-
  conf_matrix_list[
    !(conf_matrix_list[, target_dishes] %in% c("TN")),] %>%
  select(c("dataset","model", "scenario", "ingredient", target_dishes)) %>% 
  pivot_longer(cols = contains("dish"), names_to = "recipe_id") %>%
  filter(value != "TN" & scenario == "Before frame filtering") %>%
  merge( y = df_nutrition5k[, c("recipe_id", "num_ingr_per_dish") ]) %>%
  arrange(recipe_id, desc(dataset), model) %>%
  group_by(recipe_id, ingredient, num_ingr_per_dish, value) %>%
  summarise(count = n()) %>%
  mutate(flag_5_6 = ifelse(count > 4, 1, 0)) %>%
  ungroup() %>%
  group_by(recipe_id, value) %>%
  mutate(prevalence_detection = 100 * sum(flag_5_6)/num_ingr_per_dish,
         ingredient_list_by_cell = paste(ingredient, collapse = ", ")
  ) %>% 
  group_by(recipe_id) %>%
  mutate(ingredient = paste(unique(ingredient[value %in% c("FlsNeg", "TP")]), collapse = ", ")) %>% #TO SOLVE REPETITIONs
  unique() %>% 
  arrange(recipe_id, value)

tabella <-
  df_fin_reportino2.3_before_single_values %>%
  filter(recipe_id_r2 %in% target_dishes) %>%
  pivot_longer(
    cols = contains("_r"), names_to = c(".value", "publication"), names_pattern = "(.*)_(r\\d+)"
    ) %>%
  select(!publication) %>%
  pivot_longer(
    cols = matches("_(pred|gt)"), 
    names_to = c("nutrient", "type"), 
    names_pattern = "([a-z]+)_(pred|gt)"
  ) %>%
  group_by(dataset, model, scenario, nutrient, type) %>%
  summarise_at("value",     .funs = list(Min = ~ min(., na.rm = T),
                                         Q1 = ~ quantile(., 0.25, na.rm = T),
                                         Me = ~ median(., na.rm = T),
                                         Q3 = ~ quantile(., 0.75, na.rm = T),
                                         Max = ~ max(., na.rm = T),
                                         Mean = ~ mean(., na.rm = T),
                                         SD = ~ sd(., na.rm = T)
  )
  ) %>%
  regulartable() %>% autofit()
tabella

tabella_focus_target_dishes <- tabella


if(doouts == 9999){
  cat("...printing support table of nutritional profile of a subset of dishes of interest...\n")
  save_as_docx(x = tabella_focus_target_dishes, path = paste0( "./tables/", paste0(x = target_list_name, "_nutr_profile_before_reportino2_reportino3.docx") ) )
}


#### 6 - AUTOMATED FRAME FILTERING ####
if(doouts == 1){
  setwd(dir = paste(wd_main, wd_output_data, sep = "/") )
  dir.create( paste(wd_main, wd_output_data, "left_removed_frames", sep = "/") )
  
  setwd( paste(wd_main, wd_output_data, "left_removed_frames", sep = "/") )
}


## coding of frames: ##
# the matrix has 5006 rows (the number of dishes in B24 dataset) and 8880 columns (= 2 + 8878, which is the larger number of available records. 8846 is the larger number of resampled frames data)

# -1 = no n-th frame
#  0 = descarted 
#  1 = kept

### ### ### ### ### ###


dato_aff <- clean_names( read.xlsx( xlsxFile = paste(wd_main, wd_input_data, "matrix_frames_kept_full_dataset.xlsx", sep = "/") ) )


dato_aff_long <- 
  dato_aff %>%
  select( !x1 ) %>%
  pivot_longer(!recipe_id, names_to = "camera", values_to = "value") %>%
  group_by(recipe_id) %>%
  summarise(ghost_frames = sum(value == -1),
            total_frames = sum(value %in% c(0, 1) ),
            excluded_frames = sum(value == 0),
            kept_frames = sum(value == 1)
            ) %>%
  mutate(excluded_frames_ratio = 100 * excluded_frames/(excluded_frames + kept_frames),
         kept_frames_ratio = 100 * kept_frames/(excluded_frames + kept_frames),
         set = factor(case_when(recipe_id %in% training_dishes ~ "train",
                         recipe_id %in% testing_dishes ~ "test",
                         .default = "excluded or cafe2"
                         ), levels = c("excluded or cafe2", "train", "test")
                      )
         ) %>% 
  group_by(set) %>%
  mutate(total = n()) %>%
  ungroup() 


tabella <-
  dato_aff_long %>%
  #filter(recipe_id %in% intersection_before_after) %>%
  group_by(set) %>% 
  mutate_at(vars(ghost_frames:kept_frames_ratio), as.numeric) %>%
  summarise(across(ghost_frames:kept_frames_ratio,
                   .names =  "{.col}:{.fn}", 
                   .fns = list(Min = ~ min(., na.rm = T),
                               Q1 = ~ quantile(., 0.25, na.rm = T),
                               Me = ~ median(., na.rm = T),
                               Q3 = ~ quantile(., 0.75, na.rm = T),
                               Max = ~ max(., na.rm = T),
                               Mean = ~ mean(., na.rm = T),
                               SD = ~ sd(., na.rm = T),
                               n = ~ n(),
                               total = ~ sum(., na.rm = T)
                               )
                   )
            ) %>% 
  pivot_longer(everything(), names_sep = ':', names_to = c('.value', 'statistics')) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  regulartable() %>% autofit()
tabella

table_frames <- tabella


dato_aff_long %>%
  select(!recipe_id) %>%
  group_by(set) %>%
  summarise_at(vars(ghost_frames:kept_frames), .funs = list(n = sum))

# support table for a subset of dishes
target_dishes <- c("dish_1558630325", "dish_1558720236", "dish_1562703447", "dish_1563389626", "dish_1566838351",
                   "dish_1563551194", "dish_1566316757", "dish_1566849987", "dish_1568146942", "dish_1563898084",
                   "dish_1563898084", "dish_1563551194")

# focus on target dishes
dato_aff_target_dishes <- 
  dato_aff %>% select( !x1 ) %>%
  filter( recipe_id %in% target_dishes) %>%
  pivot_longer(!recipe_id, names_to = "camera", values_to = "value") %>%
  mutate( camera = str_extract(camera, "^[a-z]") ) %>%
  group_by(recipe_id, camera) %>%
  summarise(ghost_frames = sum(value == -1),
            total_frames = sum(value %in% c(0, 1) ),
            excluded_frames = sum(value == 0),
            kept_frames = sum(value == 1)
  ) %>%
  mutate(excluded_frames_ratio = 100 * excluded_frames/(excluded_frames + kept_frames),
         kept_frames_ratio = 100 * kept_frames/(excluded_frames + kept_frames),
         set = case_when(recipe_id %in% training_dishes ~ "train",
                         recipe_id %in% testing_dishes ~ "test",
                         .default = "excluded or cafe2"
         )
  ) %>% 
  mutate(set = factor(set, levels = c("excluded or cafe2", "train", "test"))) %>%
  ungroup()

if(doouts == 9999){
  cat("printing support data for statistics on frames of a subset of dishes...\n")
  write.xlsx(x = merge(x = dato_aff_long[dato_aff_long$recipe_id %in% target_dishes, -c(2, 8)],
                       y = mispred_reportino3_list[(mispred_reportino3_list$scenario == "Before frame filtering") & (mispred_reportino3_list$recipe_id %in% target_dishes), 1:4], by = "recipe_id"),
             file = "./focus_on_target_dishes.xlsx",
             sheetName = "target_dishes"
             )
}

#test: error in dishs (should be null)
dato_aff_long[dato_aff_long$total_frames != dato_aff_long$excluded_frames + dato_aff_long$kept_frames,]


tabella <-
  dato_aff_long %>%
  select(!recipe_id) %>%
  tbl_summary(
    by = set,
    label = setNames( object = as.list(gsub(x = names(dato_aff_long)[-1], pattern = "_", replacement = " ")), nm = names(dato_aff_long)[-1] ),
    percent = "column",
    missing = "ifany",
    type = list(all_continuous() ~ "continuous2"),
    statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})", "{mean} ({sd})", "{min}, {max}"),
                     all_categorical() ~ "{n} ({p}%)"
    ),
    digits = c( everything() ~ c(1, 1),
                excluded_frames_ratio ~ 2,
                kept_frames_ratio ~ 2),
    missing_text = "(Missing)"
  ) %>%
  modify_header(label ~ "Frame",
                all_stat_cols(FALSE) ~ "**{level}**, (N = {n})"
  ) %>%
  modify_spanning_header(all_stat_cols() ~ "**Database**")  %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table 
tabella

tabella_frames_excl_kept <- tabella



if(doouts == 1){
  cat("...printing table 2 of frame numbers...\n")
  save_as_docx(table_frames, path = "./table_2_frames.docx")
  #save_as_docx(tabella_frames_excl_kept, path = "./table_excl_kept_frames.docx")
  
}


## END OF CODE