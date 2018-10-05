library(googlesheets)
library(dplyr)
my_sheets = gs_ls()

my_sheets = my_sheets %>% 
  filter(as.Date(updated) > as.Date("2018-09-15")) %>% 
  filter(grepl("Fall 2018", sheet_title)) %>% 
  filter(grepl("Responses", sheet_title))

for(i in 1:length(my_sheets$sheet_title)){
  if(i == 1){
    to_read_sheet = gs_title(my_sheets$sheet_title[i])
    worksheet = "Form Responses 1"
    combined_sheet = as.data.frame(gs_read(ss=to_read_sheet, ws = worksheet))
    combined_sheet$Workshop_Type = to_read_sheet$sheet_title
  }
  else{
    to_read_sheet = gs_title(my_sheets$sheet_title[i])
    worksheet = "Form Responses 1"
    new_sheet = as.data.frame(gs_read(ss=to_read_sheet, ws = worksheet))
    new_sheet$Workshop_Type = to_read_sheet$sheet_title
    
    combined_sheet = rbind(combined_sheet, new_sheet)
  }
}

write.csv(x = combined_sheet, "Fall_Workshops.csv", row.names = F)


