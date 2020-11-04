## Read Flieshome/rahul/Shiny-ingredien

lookupPath <- "..//inputFiles//"


comma_replacements <-
  read_xlsx(paste0(lookupPath, "comma_replacements.xlsx"))  # %>% na.omit()
comma_replacements %<>% select(Ingredient) %>% mutate(Ingredient = trimws(Ingredient)) %>% unlist() %>% paste0(collapse = "|")

ingred_replacements <-
  read_xlsx(paste0(lookupPath, "ingredient_replacements.xlsx"))# %>% as.data.table(key = "Ingredient")

clubbed_file <-
  read_xlsx(paste0(lookupPath, "clubbed_ingredients.xlsx"))  ##remember updating this
club_ingred <-
  clubbed_file %>% select(clubbed_ingredients) %>% mutate(clubbed_ingredients = trimws(clubbed_ingredients)) %>%  unlist() %>% paste0(collapse = "|")


thesaurusFilePath <- paste0(lookupPath, "MasterThesaurus.csv")