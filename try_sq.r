library(tidyverse)
library(RSQLite)

d <- read_delim("~/Dropbox/TRY/TRY_Proposal_364_DataRelease_2014_09_05.txt",
                delim = "\t") 

if (file.exists("./data/try.sqlite3")) system("rm ./data/try.sqlite3")

db <- dbConnect(SQLite(), "./data/try.sqlite3", synchronous="off")

db %>% 
  dbWriteTable("try", d)
