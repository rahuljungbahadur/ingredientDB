con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 8.0 Unicode Driver",
                      Server   = "localhost",
                      UID      = "root",#rstudioapi::askForPassword("Database user"),
                      #PWD      = rstudioapi::askForPassword("Database password"),
                      Port     = 3306,
                      database = "ingredientdb")

#con <- DBI::dbConnect(RMariaDB::MariaDB(), group = "my-db")
library(dbplyr)

ingred_replacements2 <- tbl(con, "ingred_replacement") #%>% collect()

#nrow(ingred_replacements2 %>% collect())


#d1 <- dbSendQuery(con, "select * from ingred_replacement limit 5")

#dbFetch(d1)
