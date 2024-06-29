#Project Title: Data Cleaning Script In R For A Workshop Example

#R script: 20240124_nero_offspring_analysis.R
#GitHub repository: https://github.com/lebriggs/analysis-plan-canva
#Related files such as the Canva whiteboard, presentation slides, 
#and workshop instructions are found at the url below:
#https://lebriggs.com/talk/dataplan/
#Client Number: 111

#Author: Laura Briggs
#Creation Date: 24 January 2024
#Updated: 27 June 2024
#Version: 2

#Notes: 
#This is a small subset of a larger dataset
#Thank you to Jennifer Lee for providing the data to support student learning,

#Overview:
#the code below prepares (or cleans) the data for analysis
#plus a few basic counts are calculated

#I. Initial Set-up: Install And Load Required Packages With Basic Feedback

#list of required packages
pkg2 <- c("tidyverse", "here", "rvest", "gt", "gtExtras", "janitor")

#create a function to install missing packages with all dependencies
#dependencies = TRUE ensures that all required dependencies are installed
#installing all dependencies minimizes potential issues with missing packages
#adding messages provides feedback on the installation and loading status of each package

install_if_missing <- function(package) {
  tryCatch({
    if (!package %in% rownames(installed.packages())) { #check if package is installed
      install.packages(package, dependencies = TRUE) #install the package with all dependencies
      Sys.sleep(2) #add a brief delay to ensure the installation process completes properly before attempting to load the package
      .libPaths(.libPaths()) #reload the library paths to ensure that the newly installed package is recognized by the current R session
      if (!require(package, character.only = TRUE)) { #try to load the package
        return(paste("Failed to install or load package:", package)) #return message if loading fails
      } else {
        return(paste(package, "was installed and loaded successfully.")) #return message if installation and loading succeed
      }
    } else {
      library(package, character.only = TRUE) #load the package if it is already installed
      return(paste(package, "was already installed.")) #return message if the package was already installed
    }
  }, error = function(e) {
    #this function is called if an error occurs during the try block
    #e is the error object that contains details about the error
    return(paste("Error installing or loading package:", package, "-", e$message)) #extracts and returns the error message
  })
}

#install and load packages
install_results <- lapply(pkg2, install_if_missing) #apply the install_if_missing function to each package in pkg2

#print installation and loading results with a title
cat("Summary:\n", unlist(install_results), sep = "\n")

#II. Subsequent Session(s): Load Required Packages

#list of required packages that were previously installed
pkg2 <- c("tidyverse", "here", "rvest", "gt", "gtExtras", "janitor")

#load packages that were previously installed
lapply(pkg2, require, character.only = TRUE)

#where does here think the top level directory is?
#returns the highest folder level of the folder where the project file lives

here::here() #here:here() means package name::function

#III. Let's Get Nero's Offspring Data Table Into R By Doing Some Web Scraping

#read the webpage into R
#store the url as a variable
link <- "https://lebriggs.com/dataresource/2024-03-18-nero_dataset/"

#use rvest::read_html() to read all of the HTML from the webpage into R
nero_webpage <- rvest::read_html(link)

#now we need to extract the data table

#use Inspec in Chrome and "Copy the selector" for the table from the HTML element
#the table will appear highlighted on your screen
#look for something like <table class = "gt_table>
css_selector <- "#mzwvfrtvln > table"

#parse the HTML code of the table using rvest::html_element()
nero_data <- rvest::html_element(nero_webpage, css_selector)

#render the HTML code into a tibble
nero_tibble <-html_table(nero_data)
nero_tibble

#what is the structure of my data?
#I have a tibble
#19 rows and 7 observations
str(nero_tibble) 

#convert to a dataframe because I find column names to be more human readable for analyses
df_nero <- as.data.frame(nero_tibble)

#what is the structure of my data?
#I have a dataframe
#19 rows and 7 observations
#data type is mostly character class except for the ZW number
str(df_nero) 

#IV. Let's Clean Nero's Dataset!

#promote the first row of data to column headers
#delete the first row of data in the dataframe by setting remove_rows_above to TRUE
df_nero <- janitor::row_to_names(df_nero, 2, remove_rows_above = TRUE) 

#rename two columns using dplyr to make data cleaning easier
#3 and 6 are the column indices
df_nero <- dplyr::rename(df_nero, BirthDate = 3, OrthoRatings = 6)
  
#remove Nero's data from the dataframe 
#because we are interested in analyzing his offspring
#drop the first row of data
#name the new dataframe df_nero_2

df_nero_2 <- df_nero[-1, ]

#NAME

#leave the cleaning of the Name column until the end 
#because it is a bit complicated

#KKL

#KKL is a breed survey or KörKlasse
#let's create a dummy variable
#a dummy variable is either 1 or 0
#Yes equals 1; No equals 0
df_nero_2$KKL <- ifelse(df_nero_2$KKL == "Yes", 1, 0)

#count how many dogs that don't have a KKL
#15 out of 16 offspring don't have their KKL (it's hard to obtain)
count_no_kkl <- nrow(df_nero_2) - sum(df_nero_2$KKL)
count_no_kkl

#BIRTHDATE

#what class is the BirthDate column in?
#it is a character class
class(df_nero_2$BirthDate)

#convert BirthDate from character to a date class using the nifty lubridate package
#it's in the month-day-year format
df_nero_2$BirthDate <- lubridate::mdy(df_nero_2$BirthDate)

#check the class of the BirthDate column
#it's a Date class
class(df_nero_2$BirthDate)

#SEX

#Sex is a categorical variable
#convert into a factor with two levels for proper categorical analysis
df_nero_2$Sex <- factor(df_nero_2$Sex)

#what is the structure of the Sex column?
#factor with two levels: Female (1) and Male (2)
str(df_nero_2$Sex)

#number of female and male offspring
#8 females and 8 males
table_sex <- table(df_nero_2$Sex)
table_sex

#COLOUR

#normalize all the ways to say sable
df_nero_2$Colour[df_nero_2$Colour == "grey" | df_nero_2$Colour == "gray"|
                   df_nero_2$Colour == "grau"] <- "sable"

#how many unique colour values are there in the column?
#there are three values
unique(df_nero_2$Colour)

#black-tan could contain a special dash character
#replace black‑tan with blacktan
#copy the exact string you see in the output from unique() for black-tan

#gsub(pattern to look for, replacement for the pattern, the column to search) 

df_nero_2$Colour <- gsub("black‑tan", "blacktan", df_nero_2$Colour)

#a little bit of GSD colour genetics
#Nero only produces three colours: sable, black-tan, and bicolour
#he doesn't carry for black recessive
#convert as a factor
#set levels in order of colours from common to least common
df_nero_2$Colour <- factor(df_nero_2$Colour,
                           levels = c("sable", "blacktan", "bicolor"))

#what is the structure of the Colour column?
#factor with three levels: sable (1) and black-tan (2), and bicolor (3)
str(df_nero_2$Colour)

#count the number of dogs per colour using the table() function
#7 sable, 6 black-tan, and 3 bicolor
table_colour <- table(df_nero_2$Colour)
table_colour

#ORTHORATINGS
#this column contains both hip and elbow ratings

#how many unique values are there in the column?
#use the output of the function to copy the ; symbol 
#in case it's a special character
unique(df_nero_2$OrthoRatings)

#let's separate OrthoRatings into two columns at the delimiter (;)
#use str_split() from the stringr package, part of the tidyverse package
#set simplify = TRUE because we want the output as a matrix rather than a list
#the matrix will have as many columns as the maximum number of splits
#making it easy to add to a dataframe
#the matrix has two columns

split_columns_orthos <- stringr::str_split(df_nero_2$OrthoRatings, "; ", 
                                           simplify = TRUE)
split_columns_orthos

#the trimws function is used to remove any leading or trailing white spaces
df_nero_2$HipRatings <- trimws(split_columns_orthos[, 1])
df_nero_2$ElbowRatings <- trimws(split_columns_orthos[, 2])

#HIPRATINGS

#the HipRatings column contains ratings using two different rating systems 
#that we need to normalize: SV and OFA systems
#we are going to translate the OFA ratings into SV
#Excellent in OFA translates to SV a-normal
#Good in OFA translates to SV a-normal or a-fast normal
#for the purposes of this exercise:
#Good will be translated to a-normal but in real-life, we'd look at the x-rays
#(C. Kemper, personal communication, January 19, 2024)

#not going to convert the HipRatings variable into a factor 
#since the SV occasionally gives a nonstandard rating

#use the unique function just in case a-normal contains a special character
#as seems to be typical in this dataset
unique(df_nero_2$HipRatings)

df_nero_2$HipRatings[df_nero_2$HipRatings == "Excellent" |
                       df_nero_2$HipRatings == "Good" | 
                       df_nero_2$HipRatings == "a‑normal"] <-"a-normal"

#how many non-normal hip ratings do we have?

#count the number of NAs in the HipRatings column
#zero but we know this isn't correct
#what's going on here?

count_NA_hips <- sum(is.na(df_nero_2$HipRatings))
count_NA_hips

#the column is a character class so it's "NA", not a real NA with a logical class
class(df_nero_2$HipRatings)

#the simplest way to handle this issue is to change the NA at the column level
#but we could also make this change at the dataframe level
df_nero_2$HipRatings[df_nero_2$HipRatings == "NA"] <- NA

#repeat the count ofNAs
#1 NA value or 1 missing hip rating for Torque
#in real-life, we would have to try to locate the missing value
#it's very unusual to sedate a dog to only take elbow x-rays
count_NA_hips_2 <- sum(is.na(df_nero_2$HipRatings))
count_NA_hips_2

#count the number of normal hips
#create a new calculated column
df_nero_2$HipRatings_2 <- ifelse(df_nero_2$HipRatings == "a-normal", 1, 0)

#na.rm = TRUE to exclude the missing NA value
#there are 13 dogs with normal hips
count_normal_hips <- sum(df_nero_2$HipRatings_2, na.rm = TRUE)
count_normal_hips

#count of non-normal hips
#there are 2 dogs with non-normal hips
count_nonnormal_hips <- nrow(df_nero_2) - (count_normal_hips + count_NA_hips_2)
count_nonnormal_hips

#ELBOWRATINGS

#similar to hip ratings column, there are two elbow rating systems being used
#OFA and SV rating systems
#but the two systems' normal elbow ratings are comparable for this demonstration

#not going to convert the ElbowRatings variable into a factor 
#since the SV occasionally gives a nonstandard rating

#the ElbowRatings column is a character class so the same issue with NAs 
#as we saw in the HipRatings column
class(df_nero_2$ElbowRatings)

#there are zero dogs with missing elbow ratings
df_nero_2$ElbowRatings[df_nero_2$ElbowRatings == "NA"] <- NA

count_NA_elbows <- sum(is.na(df_nero_2$ElbowRatings))
count_NA_elbows

#count the number of normal elbows
#create a new calculated column
df_nero_2$ElbowRatings_2 <- ifelse(df_nero_2$ElbowRatings == "Normal", 1, 0)

#we don't have any missing elbow ratings 
#but I'll leave the logical condition set at TRUE for the future addition of data
#there are 14 dogs with normal elbows
count_normal_elbows <- sum(df_nero_2$ElbowRatings_2, na.rm = TRUE)
count_normal_elbows

#count of nonnormal elbows
#there are 2 dogs with nonnormal elbows
count_nonnormal_elbows <- nrow(df_nero_2) - (count_normal_elbows + count_NA_elbows)
count_nonnormal_elbows

#CREATE A DATAFRAME TO DISPLAY THE CALCULATED HIP AND ELBOW RATINGS VARIABLES

#we're going to make a quick visual display of the hip and elbow rating counts
#nothing fancy like the tables you can create using RMarkdown, 
#KableExtra, formattable, etc.
#let's combine the variables for the hip and elbow ratings into their own dataframe

df_nero_2orthoratings <- data.frame(count_NA_hips_2, count_normal_hips,
                                    count_nonnormal_hips, count_NA_elbows,
                                    count_normal_elbows, count_nonnormal_elbows)

#assign new names to the columns in the new dataframe 
colnames(df_nero_2orthoratings) <- c("MissingHipRatings", "NormalHips",
                                     "NonNormalHips", "MissingElbowRatings",
                                     "NormalElbows", "NonNormalElbows") 

#see the dataframe as a table in the Viewer using the gt package

#gt is for tables like ggplot2 is for charts
#use the pipe symbol to simplify stylizing the table
#read the pipe symbol as "and then"

table_orthoratings <- gt::gt(data = df_nero_2orthoratings) %>% 
  #let's add a title and subtitle
  tab_header(
    title = "Summary of Hip and Elbow Ratings for Nero vom Buchonia's Offspring", 
    subtitle = "Counts Of"
  ) %>%
  #for fun, let's style the table like in the Guardian
  gtExtras::gt_theme_guardian() %>%
  #center the title and subtitle                      
  opt_align_table_header(align = "center") %>%
  #center the values in the cells
  cols_align(align = "center", columns = everything())

#view the table in the Viewer
table_orthoratings

#ZW

#ZW is the HD breeding value calculated through a proprietary algorithm by the SV
#only available for dogs that have had their hip x-rays
#evaluated through the SV in Germany

#replace the blank cells with NAs
df_nero_2$ZW <- replace(df_nero_2$ZW, df_nero_2$ZW == "", NA)

#7 dogs have a missing ZW value
count_no_ZW <- sum(is.na(df_nero_2$ZW))
count_no_ZW

#count of dogs with ZW numbers
#9 dogs have a ZW number
count_ZW <- nrow(df_nero_2) - count_no_ZW
count_ZW

#NAME

#need to separate the show rating from the Name
#the show rating is a conformation evaluation

#we have a regular expression
#pattern <- "^(\\b[A-Z]+\\b)?\\s?(.+)$"
#the regular expression was written using ChatGPT-3.5
#use the str_match function from the stringr package
#let's apply it to an example string, "SG Nero vom Buchonia IGP3"

#the resulting matrix will have three columns
#result[, 1]: "SG Nero vom Buchonia IGP3" (entire matched substring)
#result[, 2]: "SG" (capturing group for show rating)
#result[, 3]: "Nero vom Buchonia IGP3" (capturing group for the rest of the name)

pattern <- "^(\\b[A-Z]+\\b)?\\s?(.+)$"
matrix_show <- str_match(df_nero_2$Name, pattern)
matrix_show

#if else conditional statement checks if the values in the second column of the matrix are not NA 
#if the values are not NA, it includes the values from the second column
#into a new column called ShowRating
#otherwise, it includes a NA
#the trimws function is used to remove any leading or trailing white spaces
df_nero_2$ShowRatings <- ifelse(!is.na(matrix_show[, 2]), 
                                trimws(matrix_show[, 2]), NA)

#the rest of the name from column 3 of the matrix is put into a new column
#the new column is called NameTitle
df_nero_2$NameTitle <- trimws(matrix_show[, 3])

#count how many dogs have show ratings
#we have a character class for the ShowRating column
class(df_nero_2$ShowRatings)

#convert the ShowRatings column to a factor
#the 6 show ratings are listed in order with VA being the highest
df_nero_2$ShowRatings <- factor(df_nero_2$ShowRatings,
                                levels = c("VA", "V", "SG", "G", "A", "U"))

#what is the data structure of the column?
#factor with 6 levels
str(df_nero_2$ShowRatings)

#13 dogs do not have show ratings
count_NA_showrating <- sum(is.na(df_nero_2$ShowRatings))
count_NA_showrating

#count of dogs with show ratings
#3 dogs have a show rating
count_showrating <- nrow(df_nero_2) - count_NA_showrating
count_showrating

#now we need to separate the performance titles from the dogs' names
#in the NameTitle column
#repeat the entire process that we did to create the ShowRating column
#the regular expression was written using ChatGPT

pattern_2 <- "^(.*?)(?:\\s+([[:upper:]]+\\d*))?$"

#use str_match to extract the two groups for each dog
matrix_title <- str_match(df_nero_2$NameTitle, pattern_2)
matrix_title

#create the two new columns in the df_nero_2 dataframe from columns in the matrix_title
df_nero_2$OnlyName = trimws(matrix_title[, 2])
df_nero_2$Titles = ifelse(!is.na(matrix_title[, 3]), 
                          trimws(matrix_title[, 3]), NA)

#count how many dogs have performance titles
#we have a character class for the Titles column
class(df_nero_2$Titles)

df_nero_2$Titles[df_nero_2$Titles == "NA"] <- NA

#5 dogs do not have performance titles
count_NA_title <- sum(is.na(df_nero_2$Titles))
count_NA_title

#count of dogs with performance titles
#11 dogs have a performance title
count_title <- nrow(df_nero_2) - count_NA_title
count_title

#let's combine the variables for the show ratings and performance titles 
#into their own dataframe

df_nero_2showtitles <- data.frame(count_NA_showrating, count_showrating,
                                  count_NA_title, count_title)

#assign new names to the columns in the new dataframe 
colnames(df_nero_2showtitles) <- c("NoShowRating", "HaveShowRating", "NoTitle",
                                   "HaveTitle") 

#see the dataframe as a table in the Viewer using the gt package

#gt is for tables like ggplot2 is for charts
#use the pipe symbol to simplify stylizing the table
#read the pipe symbol as "and then"

table_showtitle <- gt::gt(data = df_nero_2showtitles) %>% 
  #let's add a title and subtitle
  tab_header(
    title = "Summary of Show Ratings and Performance Titles for Nero vom Buchonia's Offspring", 
    subtitle = "Counts Of"
  ) %>%
  #for fun, let's style the table like in the Guardian
  gtExtras::gt_theme_guardian() %>%
  #center the title and subtitle
  opt_align_table_header(align = "center") %>%
  #center the values in the cells
  cols_align(align = "center", columns = everything())

#view the table in the Viewer
table_showtitle
