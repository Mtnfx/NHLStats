library(readxl) #Required for reading Excel files
library(dplyr) #Used for dataframe manipulation
library(stringr) #Used for string manipulation

files = list.files("Raw Data/") #Generate list of Excel file names
i = 0
for (file in files){ #Loop through all files and merge into single formatted dataframe
  tmp = read_excel(paste("Raw Data/", file, sep = ""),"QuantHockey", range="A2:AA84")
  tmp = tmp %>% mutate_at(vars(Rk),funs(str_sub(file,end = -6)))
  tmp <- rename(tmp,Team = Rk)
  if (i == 0){
    final = tmp2
  }
  else{
    final = bind_rows(final, tmp2)
  }
  i = i + 1
}