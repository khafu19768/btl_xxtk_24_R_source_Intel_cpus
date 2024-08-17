    library(dplyr)
    library(stringr)
    library(tidyverse)
    library(corrplot)
    library(datarium)
    library(nortest)
    library(car)
    library(ggpubr)
    library(zoo)
    library(ggplot2)
    #------------------------------------------------PREPROCESSING---------------------------------------------
    
    cpu_data = read.csv("D:/BK LEARNING/probability_and_statistic for computing/btl/archive/Intel_CPUs.csv" , na.strings = c("", "N/A")) 
    
    
    data_feature <- c('Product_Collection',
                      'Launch_Date',
                      'Lithography',
                      'Recommended_Customer_Price', 
                      'nb_of_Cores', 'nb_of_Threads', 
                      'Processor_Base_Frequency',
                      'Max_Turbo_Frequency',
                      'Cache', 'Bus_Speed','TDP',
                      'Max_Memory_Size',
                      'Max_nb_of_Memory_Channels', 
                      'Max_Memory_Bandwidth',
                      'Graphics_Base_Frequency', 
                      'Graphics_Max_Dynamic_Frequency',
                      'Graphics_Video_Max_Memory',
                      'Max_nb_of_PCI_Express_Lanes',
                      'T',
                      'Intel_Hyper_Threading_Technology_')
    
    cpu_df <- cpu_data[, data_feature, drop = FALSE]
    
    missing_data <- apply(is.na(cpu_data), 2, sum)
    print(missing_data)
    
    
    launch_date_replacements <- c('Q1\'00', 'Q2\'00', 'Q3\'00', 'Q4\'00', 'Q1\'01', 'Q2\'01', 'Q3\'01', 'Q4\'01', 'Q1\'02', 'Q2\'02', 'Q3\'02', 'Q4\'02', 'Q1\'03', 'Q2\'03', 'Q3\'03', 'Q4\'03', 'Q1\'04', 'Q2\'04', 'Q3\'04', 'Q4\'04', 'Q1\'05', 'Q2\'05', 'Q3\'05', 'Q4\'05', 'Q1\'06', 'Q2\'06', 'Q3\'06', 'Q4\'06', 'Q1\'07', 'Q2\'07', 'Q3\'07', 'Q4\'07', 'Q1\'08', 'Q2\'08', 'Q3\'08', 'Q4\'08', 'Q1\'09', 'Q2\'09', 'Q3\'09', 'Q4\'09', 'Q1\'10', 'Q2\'10', 'Q3\'10', 'Q4\'10', 'Q1\'11', 'Q2\'11', 'Q3\'11', 'Q4\'11', 'Q1\'12', 'Q2\'12', 'Q3\'12', 'Q4\'12', 'Q1\'13', 'Q2\'13', 'Q3\'13', 'Q4\'13', 'Q1\'14', 'Q2\'14', 'Q3\'14', 'Q4\'14', 'Q1\'15', 'Q2\'15', 'Q3\'15', 'Q4\'15', 'Q1\'16', 'Q2\'16', 'Q3\'16', 'Q4\'16', 'Q1\'17', 'Q2\'17', 'Q3\'17', 'Q4\'17', 'Q1\'18', 'Q2\'18', 'Q3\'18', 'Q4\'18', 'Q1 \'15', '04\'16', 'Q1\'99', 'Q2\'99')
    replacement_values <- c(seq(1, 19, 0.25), 15.75, 17.0, 0.0, 0.25)
    
    
    # Perform replacement
    cpu_df$Launch_Date <- ifelse(cpu_df$Launch_Date %in% launch_date_replacements, 
                                 replacement_values[match(cpu_df$Launch_Date, launch_date_replacements)], 
                                 cpu_df$Launch_Date)
    
    
    cpu_df <- cpu_df %>%
      mutate(Product_Collection = gsub('.*Core.*', 'Core', Product_Collection),
             Product_Collection = gsub('.*X-series.*', 'X-series', Product_Collection),
             Product_Collection = gsub('.*Celeron.*', 'Celeron', Product_Collection),
             Product_Collection = gsub('.*Pentium.*', 'Pentium', Product_Collection),
             Product_Collection = gsub('.*Quark.*', 'Quark', Product_Collection),
             Product_Collection = gsub('.*Core. [mM].*', 'm', Product_Collection),
             Product_Collection = gsub('.*Atom.*', 'Atom', Product_Collection),
             Product_Collection = gsub('.*Itanium.*', 'Itanium', Product_Collection),
             Product_Collection = gsub('.*Xeon.*', 'Xeon', Product_Collection))
    
    
    
    PriceProcessor <- function(x) {
      x <- gsub(",", "", x)
      matches <- regmatches(x, gregexpr("\\$([0-9]+(\\.[0-9]+)?)", x))
      if (any(grepl("-", x))) {
        values <- as.numeric(gsub("\\$", "", unlist(matches)))
        if (length(values) >= 2) {
          ans <- mean(values, na.rm = TRUE)
        } else {
          ans <- NA
        }
      } else if (length(matches[[1]]) > 0) {
        ans <- as.numeric(sub("\\$", "", matches[[1]][1]))
      } else {
        ans <- NA
      }
      return(ans)
    }
    
    cpu_df$Recommended_Customer_Price <- sapply(cpu_df$Recommended_Customer_Price, PriceProcessor)
    cpu_df$Recommended_Customer_Price <- as.numeric(cpu_df$Recommended_Customer_Price)
    
    
    
    
    
    # Function to convert frequency to Hz
    convert_to_hz <- function(frequency) {
      # Extract the numeric part and unit
      freq_number <- as.numeric(str_extract(frequency, "[\\d.]+"))
      freq_unit <- str_extract(frequency, "[a-zA-Z]+")
      
      # Convert to Hz
      if (grepl("GHz", freq_unit, ignore.case = TRUE)) {
        return(freq_number * 1e9) # Convert GHz to Hz
      } else if (grepl("MHz", freq_unit, ignore.case = TRUE)) {
        return(freq_number * 1e6) # Convert MHz to Hz
      } else if (grepl("Hz", freq_unit, ignore.case = TRUE)) {
        return(freq_number) # Already in Hz
      } else {
        return(NA) # Handle any unexpected cases
      }
    }
    
    
    
    
    
    
    # Function to convert memory size to Bytes
    convert_to_bytes <- function(size) {
      # Extract the numeric part and unit
      size_number <- as.numeric(str_extract(size, "[\\d.]+"))
      size_unit <- str_extract(size, "[a-zA-Z]+")
      
      # Convert to Bytes
      if (grepl("GB", size_unit, ignore.case = TRUE)) {
        return(size_number * 1e9) # Convert GB to Bytes
      } else if (grepl("MB", size_unit, ignore.case = TRUE)) {
        return(size_number * 1e6) # Convert MB to Bytes
      } else if (grepl("KB", size_unit, ignore.case = TRUE)) {
        return(size_number * 1e3) # Convert KB to Bytes
      } else if (grepl("B", size_unit, ignore.case = TRUE)) {
        return(size_number) # Already in Bytes
      } else {
        return(NA) # Handle any unexpected cases
      }
    }
    
    
    
    
    
    
    # Function to remove "°C" and convert to numeric
    remove_degree_celsius <- function(column_data) {
      # Remove "°C" and convert to numeric
      numeric_data <- as.numeric(gsub("°C", "", column_data))
      return(numeric_data)
    }
    
    
    
    
    
    
    # Function to remove "W" and convert to numeric
    remove_watt <- function(column_data) {
      # Remove "W" and convert to numeric
      numeric_data <- as.numeric(gsub("W", "", column_data))
      return(numeric_data)
    }
    
    
    
    
    
    
    # Function to remove "nm" and convert to numeric
    remove_nm <- function(column_data) {
      # Remove "nm" and convert to numeric
      numeric_data <- as.numeric(gsub("nm", "", column_data))
      return(numeric_data)
    }
    
    
    
    
    
    
    
    # Function to fill missing values in a column using the mean method
    
    fill_column_mean <- function(column_data) {
      # Calculate the mean of the non-missing values
      mean_value <- mean(column_data, na.rm = TRUE)
      
      # Fill missing values with the mean
      column_data[is.na(column_data)] <- mean_value
      
      return(column_data)
    }
    
    
    
    
    
    
    # Function to fill missing values in a column using updown method
    fill_column_updown <- function(column_data) {
      # Convert the column data to a data frame
      temp_df <- data.frame(column_data = column_data)
      
      # Fill missing values up and then down
      temp_df_filled <- temp_df %>% fill(column_data, .direction = "updown")
      
      # Return the filled column
      return(temp_df_filled$column_data)
    }
    
    
    
    
    
    
    # Function to fill missing values using the mean for Byte
    fill_missing_with_mean_byte <- function(sizes) {
      # Convert all sizes to Bytes
      sizes_bytes <- sapply(sizes, convert_to_bytes)
      
      # Calculate the mean, excluding NA values
      mean_size <- mean(sizes_bytes, na.rm = TRUE)
      
      # Replace NA values with the mean
      sizes_filled <- ifelse(is.na(sizes_bytes), mean_size, sizes_bytes)
      
      return(sizes_filled)
    }
    
    
    
    
    
    
    
    
    # Function to fill missing values using the mean
    fill_missing_with_mean <- function(frequencies) {
      # Convert all frequencies to Hz
      frequencies_hz <- sapply(frequencies, convert_to_hz)
      
      # Calculate the mean, excluding NA values
      mean_frequency <- mean(frequencies_hz, na.rm = TRUE)
      
      # Replace NA values with the mean
      frequencies_filled <- ifelse(is.na(frequencies_hz), mean_frequency, frequencies_hz)
      
      return(frequencies_filled)
    }
    
    
    
    
    # Function to convert a number to scientific notation
    convert_to_scientific <- function(number) {
      # Convert the number to scientific notation
      scientific_notation <- format(number, scientific = TRUE)
      return(scientific_notation)
    }
    
    
    
    
    
    # Apply the function to the Processor_Base_Frequency column
    cpu_df$Processor_Base_Frequency <- fill_missing_with_mean(cpu_df$Processor_Base_Frequency)
    
    # Apply the function to the Graphics_Base_Frequency column
    cpu_df$Graphics_Base_Frequency <- fill_missing_with_mean(cpu_df$Graphics_Base_Frequency)
    
    # Apply the function to the Max_Turbo_Frequency column
    cpu_df$Max_Turbo_Frequency <- fill_missing_with_mean(cpu_df$Max_Turbo_Frequency)
    
    # Apply the function to the Graphics_Max_Dynamic_Frequency column
    cpu_df$Graphics_Max_Dynamic_Frequency <- fill_missing_with_mean(cpu_df$Graphics_Max_Dynamic_Frequency)
    
    
    
    # Apply the function to the Max_Memory_Size column
    cpu_df$Graphics_Video_Max_Memory <- fill_missing_with_mean_byte(cpu_df$Graphics_Video_Max_Memory)
    
    
    # Apply the function to the Max_nb_of_PCI_Express_Lanes
    cpu_df$Max_nb_of_PCI_Express_Lanes <- fill_column_mean(cpu_df$Max_nb_of_PCI_Express_Lanes)

    
    
    
    
    # Remove "°C" and convert to numeric
    cpu_df$T <- remove_degree_celsius(cpu_df$T)
    
    # Fill missing values in the 'T' column using mean method
    cpu_df$T <- fill_column_mean(cpu_df$T)
    
    
    
    
    # Apply the function to the Max_nb_of_Memory_Channels
    cpu_df$Max_nb_of_Memory_Channels <- fill_column_updown(cpu_df$Max_nb_of_Memory_Channels)
    
    
    
    
    
    # Apply the function to the nb_of_Threads
    cpu_df$nb_of_Threads <- fill_column_updown(cpu_df$nb_of_Threads)
    
    
    
    
    
    
    
    # Remove "W" and convert to numeric
    cpu_df$TDP <- remove_watt(cpu_df$TDP)
    
    # Fill missing values in the 'TDP' column using mean method
    cpu_df$TDP <- fill_column_mean(cpu_df$TDP)
    
    
    
    
    
    
    # Remove "nm" and convert to numeric
    cpu_df$Lithography <- remove_nm(cpu_df$Lithography)
    
    # Fill missing values in the 'Lithography' column using mean method
    cpu_df$Lithography <- fill_column_mean(cpu_df$Lithography)
    
    
    
    
    
    
    
    get_numbers <- function(word) {
      if (is.character(word)) {
        return(as.numeric(str_extract(word, "[\\d]*[.]?[\\d]+")))
      } else {
        return(word)
      }
    }
    
    CacheMapper = function(x) {
      if (is.numeric(x)){
        return(x)
      } else if (grepl("K", x)){
        fac <- 1
      } else if (grepl("M", x)){
        fac <- 1000
      } else if (grepl("G", x)){
        fac <- 1000000
      } else if (grepl("T", x)){
        fac <- 1000000000
      } else {
        fac <- 1 
      }
      return(fac*get_numbers(x))
    }
    
    yes_is_1 = function(x){
      if(x == "Yes"){
        return(1);
      }else{
        return(0);
      }
    }
    
    cpu_df$Cache <- sapply(cpu_df$Cache, CacheMapper)
    cpu_df$Bus_Speed <- sapply(cpu_df$Bus_Speed, CacheMapper)
    cpu_df$Launch_Date <- as.numeric(cpu_df$Launch_Date)
    cpu_df$Max_Memory_Size <- sapply(cpu_df$Max_Memory_Size, CacheMapper)
    cpu_df$Max_Memory_Bandwidth <- gsub("[^0-9.]", "", cpu_df$Max_Memory_Bandwidth)
    cpu_df$Max_Memory_Bandwidth <- as.numeric(cpu_df$Max_Memory_Bandwidth)
    cpu_df$Product_Collection <- as.factor(cpu_df$Product_Collection)
    
    for (col in 2: ncol(cpu_df)) {
      if (class(cpu_df[[col]]) != "character") {
        cpu_df[[col]] <- ifelse(is.na(cpu_df[[col]]), mean(cpu_df[[col]], na.rm = TRUE), cpu_df[[col]])
      }
    }
    
    cpu_df <- cpu_df %>% fill(Intel_Hyper_Threading_Technology_, .direction = "updown")

    
    
    
    
    
    #-------------------------------------------------DESCRIPTIVE STATISTIC-----------------------------------------
    #summary
    summary(cpu_df)
    cor(subset(cpu_df, select = -c(Product_Collection,Intel_Hyper_Threading_Technology_)))
    corrplot(cor(subset(cpu_df, select = -c(Product_Collection,Intel_Hyper_Threading_Technology_))) ,
             number.cex = 0.5, tl.cex = 0.4,
             method = "color",
             addCoef.col = "black",
             type = "full")
    
    par(mfrow = c(1, 2))
    
    #Diagram of Launch date
    hist(cpu_df$Launch_Date,
         breaks = 50, 
         xlim = c(0,20),
         ylim = c(0,600), 
         xlab = "", ylab = "Frequency", 
         main = "Histogram of Launch_Date",
         col = "pink", 
         border = "black")
    boxplot(cpu_df$Launch_Date, main = "Boxplot of Launch_Date")
    
    #Diagram of Recommended customer price
    hist(cpu_df$Recommended_Customer_Price, 
         breaks = 50,
         xlab = "Dollar", 
         ylab = "Frequency", 
         xlim = c(0, 15000),
         ylim = c(0, 1100),
         main = "Histogram of \n Recommended_Customer_Price",
         col = "pink", 
         border = "black")
    boxplot(cpu_df$Recommended_Customer_Price, main = "Boxplot of \n Recommended_Customer_Price")
    
    #Diagram of nb of cores
    hist(cpu_df$nb_of_Cores,
         breaks = 50, 
         xlab = "Core",
         ylab = "Frequency",
         xlim = c(0, 80),
         ylim = c(0, 1400),
         main = "Histogram of nb_of_Cores",
         col = "pink", 
         border = "black")
    boxplot(cpu_df$nb_of_Cores, main = "Boxplot of nb_of_Cores")
    
    #Diagram of nb of threads
    hist(cpu_df$nb_of_Threads,
         breaks = 50,
         xlab = "Thread",
         ylab = "Frequency", 
         xlim = c(0, 60),
         ylim = c(0, 1000), 
         main = "Histogram of nb_of_Threads",
         col = "pink", 
         border = "black")
    boxplot(cpu_df$nb_of_Threads, main = "Boxplot of nb_of_Threads")
    
    
    
    
    
    
    # Create a histogram of Processor_Base_Frequency
    hist(cpu_df$Processor_Base_Frequency, 
         breaks = 50, 
         xlim = c(0, 4.300e+09), 
         ylim = c(0, 150), 
         xlab = "Hz", 
         ylab = "Frequency", 
         main = "Histogram of \n Processor_Base_Frequency", 
         col = "pink", 
         border = "black")
    
    # Create a box plot of Processor_Base_Frequency
    boxplot(cpu_df$Processor_Base_Frequency, 
            main = "Boxplot of \n Processor_Base_Frequency", 
            ylab = "Frequency (Hz)", 
            col = "lightblue",
            border = "black")
    
    
    
    
    
    
    
    # Create a histogram of Max_Turbo_Frequency
    hist(cpu_df$Max_Turbo_Frequency, 
         breaks = 50, 
         xlim = c(0, 4.500e+09), 
         ylim = c(0, 150), 
         xlab = "Hz", 
         ylab = "Frequency", 
         main = "Histogram of \n Max_Turbo_Frequency", 
         col = "pink", 
         border = "black")
    
    # Create a box plot of Max_Turbo_Frequency
    boxplot(cpu_df$Max_Turbo_Frequency, 
            main = "Boxplot of \n Max_Turbo_Frequency", 
            ylab = "Frequency (Hz)", 
            col = "lightblue",
            border = "black")
    
    
    
    
    
    
    
    
    # Create a histogram of Graphics_Base_Frequency
    hist(cpu_df$Graphics_Base_Frequency, 
         breaks = 50, 
         xlim = c(0, 20000000000 ),  
         ylim = c(0, 3000), 
         xlab = "Hz", 
         ylab = "Frequency", 
         main = "Histogram of \n Graphics_Base_Frequency", 
         col = "pink", 
         border = "black")
    
    # Create a box plot of Graphics_Base_Frequency
    boxplot(cpu_df$Graphics_Base_Frequency, 
            main = "Boxplot of \n Graphics_Base_Frequency", 
            ylab = "Frequency (Hz)", 
            col = "lightblue",
            border = "black")
    
    
    
    
    
    # Create a histogram of Graphics_Max_Dynamic_Frequency
    hist(cpu_df$Graphics_Max_Dynamic_Frequency, 
         breaks = 50, 
         xlim = c(0, 1.350e+09), 
         ylim = c(0, 150), 
         xlab = "Hz", 
         ylab = "Frequency", 
         main = "Histogram of \n Graphics_Max_Dynamic_Frequency", 
         col = "pink", 
         border = "black")
    
    # Create a box plot of Graphics_Max_Dynamic_Frequency
    boxplot(cpu_df$Graphics_Max_Dynamic_Frequency, 
            main = "Boxplot of \n Graphics_Max_Dynamic_Frequency", 
            ylab = "Frequency (Hz)", 
            col = "lightblue",
            border = "black")
    
    
    
    
    # Create a histogram of Graphics_Video_Max_Memory
    hist(cpu_df$Graphics_Video_Max_Memory, 
         breaks = 50, 
         xlim = c(0, 6.40e+10), 
         ylim = c(0, 150), 
         xlab = "Bytes", 
         ylab = "Frequency", 
         main = "Histogram of \n Graphics_Video_Max_Memory", 
         col = "pink", 
         border = "black")
    
    # Create a box plot of Graphics_Video_Max_Memory
    boxplot(cpu_df$Graphics_Video_Max_Memory, 
            main = "Boxplot of \n Graphics_Video_Max_Memory", 
            ylab = "Memory Size (Bytes)", 
            col = "lightblue",
            border = "black")
    
    
    
    
    
    
    #Diagram of Cache
    hist(cpu_df$Cache,
         breaks = 50,
         xlab = "Byte",
         ylab = "Frequency",
         ylim = c(0, 600),
         main = "Histogram of Cache",
         col = "pink", 
         border = "black")
    boxplot(cpu_df$Cache, main = "Boxplot of Cache")

    
    #Diagram of Bus speed
    hist(cpu_df$Bus_Speed,
         breaks = 50,
         xlim = c(0, 9600000 ),
         ylim = c(0, 1000), 
         xlab = "Hz/s",
         ylab = "Frequency",
         main = "Histogram of Bus_Speed")
    boxplot(cpu_df$Bus_Speed, main = "Boxplot of Bus_Speed")
    
    #Diagram of Max memory size
    hist(cpu_df$Max_Memory_Size,
         breaks = 50,
         ylim = c (0, 1000), 
         xlab = "Byte",
         ylab = "Frequency",
         main = "Histogram of \n Max_Memory_Size")
    boxplot(cpu_df$Max_Memory_Size, main = "Boxplot of \n Max_Memory_Size")
    
    #Diagram of Max memory bandwidth
    hist(cpu_df$Max_Memory_Bandwidth,
         breaks = 50,
         ylim = c (0, 1400),
         xlab = "GB",
         ylab = "Frequency",
         main = "Histogram of \n Max_Memory_Bandwidth")
    boxplot(cpu_df$Max_Memory_Bandwidth, main = "Boxplot of \n Max_Memory_Bandwidth")
    
    #find confidence interval
    t.test(cpu_df $Graphics_Max_Dynamic_Frequency, conf.level = 0.99)
    
    
    #a cpus has a recomended price is not less than 800$ with confidence level = 99%
    t.test(cpu_df $ Recommended_Customer_Price , alternative =  "less" , mu = 800, conf.level = 0.99)
    
    #there is no difference between the recommended price of having Intel Hyper threading technology and not having it
     t.test(Recommended_Customer_Price ~ Intel_Hyper_Threading_Technology_, data = cpu_df, conf.level = 0.99)
    
    
    skip_columns <- c('Product_Collection','Intel_Hyper_Threading_Technology_')
    
    for (col in names(cpu_df)) {
      if (!(col %in% skip_columns)) {
        test_result <- shapiro.test(cpu_df[[col]])
        cat("Shapiro-Wilk test for", col, "\n")
        print(test_result)
        cat("\n")
      }
    }
    
    
    
    skip_columns <- c('Product_Collection','Intel_Hyper_Threading_Technology_')
    for (col in names(cpu_df)) {
      if (!(col %in% skip_columns)) {
        test_result <- leveneTest(cpu_df[[col]] ~ as.factor(cpu_df $Product_Collection ))
        cat("leveneTest test for", col, "\n")
        print(test_result)
        cat("\n")
      }
    }
    
    
    # hoi quy tuyen tinh nhieu bien 
    mod_multiple_factors <- lm(Recommended_Customer_Price ~  cpu_df $Launch_Date
                + cpu_df $Lithography
                + cpu_df $nb_of_Cores
                + cpu_df $nb_of_Threads
                + cpu_df $Processor_Base_Frequency
                + cpu_df $Cache
                + cpu_df $Bus_Speed
                + cpu_df $TDP
                + cpu_df $Max_Memory_Size
                + cpu_df $Max_nb_of_Memory_Channels
                + cpu_df $Max_Memory_Bandwidth
                + cpu_df $Graphics_Base_Frequency
                + cpu_df $Graphics_Max_Dynamic_Frequency
                + cpu_df $Graphics_Video_Max_Memory
                + cpu_df $Max_nb_of_PCI_Express_Lanes
                + cpu_df $T, data  = cpu_df)
    summary(mod_multiple_factors)
   
    
    
    
    require(ggpubr)
    ggscatter(data = cpu_df , y = "Cache" , x =  "nb_of_Threads") + geom_smooth(method = "lm" , se = TRUE)
    modtest_1_factor <- lm(cpu_df $Recommended_Customer_Price ~ cpu_df $Launch_Date, data = cpu_df)
    summary(modtest_1_factor)
    plot(modtest_1_factor)
    
    modtest_1_factor <- lm(cpu_df $Bus_Speed ~ cpu_df $Cache  , data = cpu_df)
    summary(modtest_1_factor)
    
    mod1 <-lm(cpu_df $ Recommended_Customer_Price ~ cpu_df $Max_nb_of_Memory_Channels,data = cpu_df)
    summary(mod1)
    plot(mod1)
    
    
    
    
    