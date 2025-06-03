library(readxl) #Required for reading Excel files
library(dplyr) #Used for dataframe manipulation
library(stringr) #Used for string manipulation

files = list.files("Raw Data/") #Generate list of Excel file names
i = 0 #This is a counter that tells us how many sheets we have already read.
for (file in files){ #Loop through all files and merge into single formatted dataframe
  tmp = read_excel(paste("Raw Data/", file, sep = ""),"QuantHockey", range="A2:AA84") #We want to read the second sheet (the one with data on it) and set the reading range to ignore the top headings.
  tmp = tmp %>% mutate_at(vars(Rk),funs(str_sub(file,end = -6))) #We don't need the game number, replace this with the team the data corresponds to
  tmp <- rename(tmp,Team = Rk) #Relabel column to reflect name data
  if (i == 0){
    final = tmp #Initialize final data for data of first spreadsheet
  }
  else{
    final = bind_rows(final, tmp) #For all subsequent spreadsheets, append formatted data to final dataframe
  }
  i = i + 1
}

#Hyphens throw several errors when we try to use various dplyr functions. We replace any hyphens
for (name in names(final)){
  names(final)[names(final) == name] = str_replace_all(name,"-","_")
  name = str_replace_all(name,"-","_")
  names(final)[names(final) == name] = str_replace_all(name,"%","_PCT")
}

final =  mutate_at(final,vars(PDO,PDO_A,SH_PCT,SH_PCT_A,FO_PCT,SV_PCT,SV_PCT_A), funs(as.numeric(str_sub(.,end=-2))))

write.table(final, "clean_data.csv", row.names = FALSE, sep=",") #This is the full clean data

#To generate a dataset better to use in Python, we want only numbers
final = final %>% mutate_at(vars(Loc.), funs(if_else(Loc. == "Home", 1, 0))) #Convert Home/Away to numerical values so we can use these in machine learning models

final = final %>% mutate(Team = NULL, Date = NULL, Opponent = NULL, Result = NULL) #Remove all remaining string-valued columns

final = final %>% select(GF, GA, GD, everything()) #Bring GF, GA, and GD to front of dataframe so we can easily split them from the rest of the data in Python

write.table(final, "clean_data_trim.csv", row.names = FALSE, col.names = FALSE, sep=",") #This is the clean data with only numerical values, intended for use in Python
