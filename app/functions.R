## 
#source("database.R")
source("libraries.R")

## Custom Functions


prepare_text <- function(ingrid_values){   ##This function finds out the position of all the brackets and commas
  ##Remove information before ":"
  
  #Need to know whether the braces have any comma in between them
  ##For comma
  comma_list1 <- gregexpr(",", ingrid_values)[[1]]
  names(comma_list1) <- rep(",", length(comma_list1))
  
  ##For open round brackets
  round_openb_list <- gregexpr("\\(", ingrid_values, perl = T)[[1]]
  #round_openb_list <- gregexpr("\\((?=([^\\)]+,))", ingrid_values, perl = T)[[1]] ##Changed 06_17_2020
  names(round_openb_list) <- rep("(", length(round_openb_list))
  
  ##For close round brackets
  round_closeb_list <- gregexpr("\\)", ingrid_values, perl = T)[[1]]
  #closeb_matches <- gregexpr(",[^\\(\\)]+\\){,1}", ingrid_values)[[1]] ##Added 06_17_2020
  #round_closeb_list <- closeb_matches + attr(closeb_matches, which = "match.length") - 1     ##Added 06_17_2020
  names(round_closeb_list) <- rep(")", length(round_closeb_list))
  
  ##appending final list
  brackets <- c(comma_list1, round_openb_list, round_closeb_list, str_length(ingrid_values))
  brackets %<>% sort(decreasing = F)
  brackets <-  brackets[brackets > 0] ##Ensuring that the characters not found are removed from the analysis
  
  
  return(brackets)
  rm(brackets)
}
############################

ingid_list_prep <- function(brackets, ingrid_values, braces, neg_braces, ean_no){    ##Based on the results of the "prepare_text" function, this function separates the ingredients
  list_of_ingrids <- list()
  i = 1
  k = 1
  
  
  ##Ingredient replacement and comma replacement hould be applied to both Clubbed ingredients and non-brackets.
  
  ##reading the CLUBBED INGREDIENTS file
  
  
  sub_ingred <- gregexpr(pattern = club_ingred, text = ingrid_values, ignore.case = T)
  #print(sub_ingred[[1]][1])
  #browser()
  if(sub_ingred[[1]][1] != -1){  ##A match is found
    ingrid_values <- gsub(ingrid_values, pattern = club_ingred, replacement = "", ignore.case = T, perl = T)
    #print("found_it")
    #browser()
    p = 0
    for (sub_ingreds in 1:length(sub_ingred[[1]])) {
      
      matched = sub_ingred[[1]][sub_ingreds] + attr(sub_ingred[[1]],'match.length')[sub_ingreds] + p
      
      brackets <- brackets[-match(matched-1, brackets)]
      brackets[which(brackets > matched)] <- brackets[which(brackets > matched)] - attr(sub_ingred[[1]],'match.length')[sub_ingreds]
      p = p - attr(sub_ingred[[1]],'match.length')[sub_ingreds]
    }
    
  }
  
  #print(brackets)
  while(i <= length(brackets)){
    if(!names(brackets)[i] %in% names(braces)){  ##the ingredients are not within parenthesis
      #print(ingrid_values)
      
      
      list_of_ingrids[[i]] <- substr(ingrid_values, k, brackets[i]-1) %>% trimws() %>% tolower()
      
    }else {
      ##Start of modification 07_17_2020
      ##Old code
      # if(names(brackets)[i] %in% names(braces)){
      # 
      # 
      # counter <- braces[names(brackets)[i]]
      # #browser()
      # while(counter != 0){   ##run loop till matching bracket found
      #     
      #     i= i+1
      #     counter <- sum(braces[names(brackets)[i]],neg_braces[names(brackets)[i]], counter, na.rm = T)
      #     if(i > length(brackets)) {
      #         #fails_list <- c(fails_list, ean_no)
      #         break()
      #     }
      #     
      # }
      # if (names(brackets)[i] != "," & !is.na(names(brackets)[i])){# & names(brackets)[i] != "("){ #Modified 07_17_2020
      #     
      #     i = i+1
      # }##End of modification 07_17_2020
      # list_of_ingrids[[i]] <- substr(ingrid_values, k, brackets[i] - 1) %>% trimws() %>% tolower()
      ## End of old code
      #browser()
      while (i <= length(brackets) & names(brackets[i]) != ",") {
        if (names(brackets[i]) == "(") {
          counter <- braces[names(brackets)[i]]
          while (counter != 0) {   ##run loop till matching bracket found
            i = i + 1
            counter <- sum(braces[names(brackets)[i]],neg_braces[names(brackets)[i]], counter, na.rm = T)
            if(i > length(brackets)) {
              #fails_list <- c(fails_list, ean_no)
              break()
            }
          }   
        }
        i = i +1
      }
      list_of_ingrids[[i]] <- substr(ingrid_values, k, brackets[i] - 1) %>% trimws() %>% tolower()
    } ##End of modification
    if (names(brackets)[i] %in% names(neg_braces)){
      names(brackets[i]) <- ","
      substr(ingrid_values, brackets[i], brackets[i]) <- ","
    }
    k = brackets[i] + 1
    i = i+1
  }
  
  #########
  
  list_of_ingrids[sapply(list_of_ingrids, is.null)] <- NULL
  list_of_ingrids[list_of_ingrids == ""] <- NULL
  if (length(list_of_ingrids) == 0) {
    #browser()
    list_of_ingrids[1] <- NA
  }
  names(list_of_ingrids) <- paste0("ingredient", seq(1,length(list_of_ingrids)))
  func2_output <- list("list_of_ingrids" = list_of_ingrids)
  #return(list_of_ingrids)
  #abort(print(ean_no))
  return(func2_output)
  rm(list_of_ingrids)
  
}


###########################
ingid_list_prep_lvl2 <- function(brackets, ingrid_values, braces, neg_braces, ean_no){    ##Based on the results of the "prepare_text" function, this function separates the ingredients
  list_of_ingrids <- list()
  
  
  ##Separating ingredients
  ingrid_values %<>% gsub(pattern = "\\(|\\)", replacement = ",")
  list_of_ingrids <- str_split(ingrid_values, pattern = ",")[[1]] %>% trimws() %>% as.list()
  
  
  
  
  
  #########
  #browser()
  list_of_ingrids[sapply(list_of_ingrids, is.null)] <- NULL
  list_of_ingrids[list_of_ingrids == ""] <- NULL
  if (length(list_of_ingrids) == 0) {
    list_of_ingrids[1] <- NA
  }
  #list_of_ingrids <- ifelse(length(list_of_ingrids) == 0, list_of_ingrids[1] <- NA, list_of_ingrids)
  names(list_of_ingrids) <- paste0("ingredient", seq(1,length(list_of_ingrids)))
  #browser()
  func2_output <- list("list_of_ingrids" = list_of_ingrids)
  #return(list_of_ingrids)
  #abort(print(ean_no))
  return(func2_output)
  rm(list_of_ingrids)
  
}

### End of ingredient list prep level 2

ingrid_sep <- function(dataframe, IDcol, Valcol, updateProgress = NULL){   ##This function combines the ingredient list from all the foods (_main_ function)
  
  #dataframe <- gsub("\\{|\\[", replacement = "(", dataframe)
  
  braces <- c(1,1,1)
  names(braces) <- c("(", "{", "[")
  neg_braces <- c(-1, -1, -1)
  names(neg_braces) <- c(")", "}", "]")
  
  len_df <- nrow(dataframe)
  #final_list <- data.frame()
  final_list <- vector(mode = "list", length = len_df)
  failed_ean_list <- c()
  #for(l in 1:len_df){
  #library(doParallel)
  # cores = detectCores()
  # cl <- makeCluster(cores[1] - 3)
  # 
  # registerDoParallel(cl)
  # 
  # foreach(l = 1:len_df) %dopar% {
    
  for (l in 1:len_df) {
    
    if (is.function(updateProgress)) {
      text <- paste0("Rows processed = ", l)
      updateProgress(value = l, detail = text)
    }
    ean_no <- dataframe[[IDcol]][l]
    
    temp <- dataframe[[Valcol]][l]
    
    #temp <- sub(pattern = "^.*?!=\\,:", perl = T, replacement = "" , temp)   ###Solve this
    temp <- gsub("\\{|\\[", replacement = "(", temp)
    temp <- gsub("\\}|\\]", replacement = ")", temp)
    #temp <- gsub(pattern = ", \\(", replacement = " \\(", temp)
    #temp <- gsub(pattern = ",\\(", replacement = " \\(", temp)      
    temp <- gsub("\\*|\\*\\*", "", temp)
    temp <- gsub("\\+|\\+\\+", "", temp)
    for (ing_repl in 1:nrow(ingred_replacements)) {
      #browser()
      #print(without_brackets)
      temp <- gsub(pattern = ingred_replacements$Ingredient[ing_repl],
                   replacement = ingred_replacements$Replacement[ing_repl], temp, ignore.case = T, perl = T)

    }
    # cl <- makeCluster(3)
    # registerDoParallel(cl)
    # foreach(ing_repl = 1:80) %dopar%#nrow(ingred_replacements)) %dopar%
    #   temp <- gsub(pattern = ingred_replacements$Ingredient[ing_repl],
    #                replacement = ingred_replacements$Replacement[ing_repl], temp, ignore.case = T, perl = T)
    
    ###SQL Query
    # for (ing_repl in 1:nrow(ingred_replacements2)) {
    #   #browser()
    #   #print(without_brackets)
    #   temp <- gsub(pattern = ingred_replacements2$ingredient[ing_repl],
    #                replacement = ingred_replacements2$replacement[ing_repl], temp, ignore.case = T)
    #   
    # }
    
    ### End of SQL query
    
    #temp <- ingred_replacements %>% filter(ingredient == temp) %>% select(replacement)
    temp <- gsub(pattern = "\\sand\\s(\\w*$)", replacement = ",\\1", ignore.case = T, temp)   ##Replaces and with ,(comma) if the pattern is ' word and word[end]'
    temp <- gsub(pattern = "\\sand\\s(\\w*\\.$)", replacement = ",\\1", ignore.case = T, temp)##Replaces and with ,(comma) if the pattern is ' word and word.[end]'
    
    temp <- paste0(temp, ifelse(grepl(pattern = "\\.$", x = temp), "", "."), sep = "")  ###Adds a "." at the end of the description of each food
    temp <- gsub("\\.|;", ",", temp)    ##Changed: commented on 5/28/2020
    ingd0_prep <- prepare_text(temp)
    #browser()
    ### Fails List: if the number of opening and closing brackets don't match add it to fails_list
    if(sum(braces[names(ingd0_prep)],neg_braces[names(ingd0_prep)], na.rm = T) != 0){
      failed_ean_list <- c(failed_ean_list, ean_no)
    }
    sub_ingred <- gregexpr(pattern = club_ingred, text = temp, ignore.case = T)
    
    #print(sub_ingred[[1]][1])
    #browser()
    if(sub_ingred[[1]][1] != -1){  ##A match is found
      temp <- gsub(temp, pattern = club_ingred, replacement = "", ignore.case = T)
      #print("found_it")
      #browser()
      p = 0
      for (sub_ingreds in 1:length(sub_ingred[[1]])) {
        
        matched = sub_ingred[[1]][sub_ingreds] + attr(sub_ingred[[1]],'match.length')[sub_ingreds] + p
        
        ingd0_prep <- ingd0_prep[-match(matched-1, ingd0_prep)]
        ingd0_prep[which(ingd0_prep > matched)] <- ingd0_prep[which(ingd0_prep > matched)] - attr(sub_ingred[[1]],'match.length')[sub_ingreds]
        p = p - attr(sub_ingred[[1]],'match.length')[sub_ingreds]
      }
      
    }
    k = 1
    i = 1
    without_brackets <- list()
    with_brackets <- list()
    while(i <= length(ingd0_prep)){
      if(!names(ingd0_prep)[i] %in% names(braces)){  ##the ingredients are not within parenthesis
        #print(temp)
        without_brackets <- append(without_brackets, substr(temp, k, ingd0_prep[i]-1) %>%
                                     #trimws() %>%
                                     tolower())
        
      }
      
      
      else if(names(ingd0_prep)[i] %in% names(braces)){
        #browser()
        ##New changes 12/30
        outside_braces1 <- substr(temp, k, ingd0_prep[i] - 1) #%>% trimws() %>%  tolower()
        outside_braces <- outside_braces1
        len_out_braces <- nchar(outside_braces)
        ##Replacing ingredients outside brackets
        # for(ing_repl in 1:nrow(ingred_replacements)){
        #     #print(outside_braces)
        #     outside_braces <- gsub(pattern = as.character(ingred_replacements$Ingredient[ing_repl]),
        #                            replacement = as.character(ingred_replacements$Replacement[ing_repl]), outside_braces, ignore.case = T)
        # 
        # }
        #browser()
        outside_braces <- gsub("\\.|;", ",", outside_braces)     ##Changed: commented on 5/28/2020
        outside_braces <- gsub("\\*|\\*\\*", "", outside_braces)
        outside_braces1 <- gsub("\\*|\\*\\*", "", outside_braces1)
        outside_braces1 <- gsub("\\+|\\+\\+", "", outside_braces1)
        outside_braces <- gsub("\\+|\\+\\+", "", outside_braces)
        outside_braces <- gsub(comma_replacements, replacement = ",", outside_braces, ignore.case = T)
        ##New K is the last comma in outside_braces + 1
        #print(nchar(outside_braces))
        #substr(temp, k, ingd0_prep[i] - 1) <- paste(outside_braces, paste0(collapse = "", rep(" ",abs(len_out_braces - nchar(outside_braces)))))
        #replacement <- paste(outside_braces, paste0(collapse = "", rep(" ",abs(len_out_braces - nchar(outside_braces)))))
        #print(ean_no)
        temp <- sub(pattern = outside_braces1, replacement = outside_braces, temp)
        last_comma <- gregexpr(pattern = ",", outside_braces)[[1]][length(gregexpr(pattern = ",", outside_braces)[[1]])]
        k <- sum(c(k,last_comma), na.rm = T)
        without_brackets <- append(without_brackets, substr(outside_braces, 0, last_comma - 1))
        #print(ean_no)
        ##End of new change 12/30
        #browser()
        ##Commented on 07_17_2020
        # counter <- braces[names(ingd0_prep)[i]]
        # 
        # while(counter != 0){   ##run loop till matching bracket found
        #     
        #     i= i+1
        #     counter <- sum(braces[names(ingd0_prep)[i]],neg_braces[names(ingd0_prep)[i]], counter, na.rm = T)
        #     if(i > length(ingd0_prep)) {
        #         #fails_list <- c(fails_list, ean_no)
        #         break()
        #     }
        #     
        # }
        # ##Added 06_18_2020 for handling ingredients of pattern "text (text1, text2) text3,"
        # if (names(ingd0_prep)[i] != ","& !is.na(names(ingd0_prep)[i]) & names(ingd0_prep)[i] != "("){
        #     i = i+1
        # }
        ##End of Comments 07_17_2020
        ##New Changes 07_17_2020
        #browser()
        while (i <= length(ingd0_prep) & names(ingd0_prep[i]) != ","){
          if(names(ingd0_prep[i]) == "("){
            counter2 <- braces[names(ingd0_prep)[i]]
            while(counter2 != 0){   ##run loop till matching bracket found
              i= i+1
              counter2 <- sum(braces[names(ingd0_prep)[i]],neg_braces[names(ingd0_prep)[i]], counter2, na.rm = T)
              if(i > length(ingd0_prep)) {
                #fails_list <- c(fails_list, ean_no)
                break()
              }
            }
          }
          i = i+1
        }
        ##End of New Changes
        ##End of addition
        ##Added 1/6/2020 - to remove extra spaces being put into the with_brackets terms
        #browser()
        ingd0_prep <- ingd0_prep + (nchar(outside_braces) - nchar(outside_braces1))
        with_brackets <- append(with_brackets, substr(temp, k, ingd0_prep[i] - 1) %>% gsub(pattern = "\\s{2,}", replacement = " ") %>% trimws() %>% tolower())
        ##with_brackets <- append(with_brackets, substr(temp, k, ingd0_prep[i]) %>% trimws() %>% tolower()) ##Original code
        ##End of addition
        
      }else if(names(ingd0_prep)[i] %in% names(neg_braces)){
        names(ingd0_prep[i]) <- ","
        substr(temp, ingd0_prep[i], ingd0_prep[i]) <- ","
      }
      k = ingd0_prep[i] + 1
      i = i+1
    }
    #browser()
    without_brackets <- paste0(na.omit(without_brackets), collapse = ",") 
    #print(without_brackets)
    # for(ing_repl in 1:nrow(ingred_replacements)){
    #     #browser()
    #     #print(without_brackets)
    #     without_brackets <- gsub(pattern = as.character(ingred_replacements$Ingredient[ing_repl]),
    #                              replacement = as.character(ingred_replacements$Replacement[ing_repl]), without_brackets, ignore.case = T)
    #     
    # }
    #browser()
    without_brackets <- gsub("\\.|;", ",", without_brackets)    ##Changed: removed ":" on 5/28/2020
    without_brackets <- gsub(comma_replacements, replacement = ",", without_brackets, ignore.case = T)
    #without_brackets <- gsub(pattern = "'and'", replacement = 'and', without_brackets, ignore.case = T)
    with_brackets <- paste0(na.omit(with_brackets), collapse = ",")
    #with_brackets <- gsub(pattern = "'and'", replacement = 'and', with_brackets, ignore.case = T)
    temp <- paste(without_brackets, with_brackets, sep = ",")
    temp <- paste0(temp, ifelse(grepl(pattern = "\\.$", x = temp), "", "."), sep = "")  ###Adds a "." at the end of the description of each food
    temp <- gsub("\\.|;|:", ",", temp)  ##Changed: removed ":" on 5/28/2020
    #browser()
    
    #browser()
    temp <- gsub(pattern = "'and'", replacement = 'and', temp, ignore.case = T)
    temp <- gsub(pattern = "'&'", replacement = '&', temp, ignore.case = T)
    temp <- gsub(pattern = "'and/or'", replacement = 'and/or', temp, ignore.case = T)
    temp <- gsub(pattern = ",{2,}", replacement = ",", temp)
    temp <- gsub("\\*|\\*\\*", "", temp)
    temp <- gsub("\\+|\\+\\+", "", temp)
    #print(temp)
    rm(without_brackets)
    rm(with_brackets)
    #temp <- gsub(comma_replacements, replacement = ",", temp, ignore.case = T)
    
    #print(temp)
    ingd1_prep <- prepare_text(temp)
    #browser()
    
    func2_output <- ingid_list_prep(brackets = ingd1_prep, ingrid_values = temp, braces, neg_braces, ean_no)
    rm(temp)
    list_ingid <- func2_output$list_of_ingrids
    #failed_ean_list <- failed_ean_list  ##Gets the list of EANs that failed from func2
    
    final_list <- bind_rows(final_list, list_ingid)
    
    
    #abort(print(ean_no))
    
  }
  
  #stopCluster(cl)
  
  failed_ean_list <- failed_ean_list[!is.null(failed_ean_list)]
  
  final2_list <- cbind(dataframe[[IDcol]][1:len_df], final_list)
  
  colnames(final2_list)[1] <- IDcol
  #View(final2_list %>% filter(IDcol %in% failed_ean_list))
  final2_list %<>% filter(!IDcol %in% failed_ean_list)
  
  
  func3_output <- list("combined_list" = as.data.frame(final2_list), "failed_ean" = failed_ean_list)
  return(func3_output)
}


####ingredient separator for level 2
ingrid_sep_lvl_2 <- function(dataframe, IDcol, Valcol, updateProgress = NULL){   ##This function combines the ingredient list from all the foods (_main_ function)
  
  #dataframe <- gsub("\\{|\\[", replacement = "(", dataframe)
  
  braces <- c(1,1,1)
  names(braces) <- c("(", "{", "[")
  neg_braces <- c(-1, -1, -1)
  names(neg_braces) <- c(")", "}", "]")
  
  len_df <- nrow(dataframe)
  
  final_list <- data.frame()
  failed_ean_list <- c()
  for(l in 1:len_df){
    
    if (is.function(updateProgress)) {
      text <- paste0("Rows processed = ", l)
      updateProgress(value = l, detail = text)
    }
    ean_no <- dataframe[[IDcol]][l]
    
    temp <- dataframe[[Valcol]][l]
    
    #temp <- sub(pattern = "^.*?!=\\,:", perl = T, replacement = "" , temp)   ###Solve this
    temp <- gsub("\\{|\\[", replacement = "(", temp)
    temp <- gsub("\\}|\\]", replacement = ")", temp)
    #temp <- gsub(pattern = ", \\(", replacement = " \\(", temp)
    #temp <- gsub(pattern = ",\\(", replacement = " \\(", temp)
    #temp <- gsub(pattern = "[^,\\(]+\\([^\\)")
    
    # for(ing_repl in 1:nrow(ingred_replacements_lvl2)){
    #     #browser()
    #     #print(without_brackets)
    #     temp <- gsub(pattern = as.character(ingred_replacements_lvl2$Ingredient[ing_repl]),
    #                  replacement = as.character(ingred_replacements_lvl2$Replacement[ing_repl]), temp, ignore.case = T)
    #     
    # }
    temp <- gsub(pattern = "\\sand\\s(\\w*$)", replacement = ",\\1", ignore.case = T, temp)   ##Replaces 'and' with ,(comma) if the pattern is ' word and word[end]'
    temp <- gsub(pattern = "\\sand\\s(\\w*\\.$)", replacement = ",\\1", ignore.case = T, temp)##Replaces 'and' with ,(comma) if the pattern is ' word and word.[end]'
    temp <- gsub(pattern = "[^\\(,]+\\([^\\),]+\\)", replacement = "", ignore.case = T, temp)  ##Removes all brackets
    temp <- paste0(temp, ifelse(grepl(pattern = "\\.$", x = temp), "", "."), sep = "")  ###Adds a "." at the end of the description of each food
    temp <- gsub("\\.|;|:", ",", temp)
    ingd0_prep <- prepare_text(temp)
    ### Fails List: if the number of opening and closing brackets don't match add it to fails_list
    if(sum(braces[names(ingd0_prep)],neg_braces[names(ingd0_prep)], na.rm = T) != 0){
      failed_ean_list <- c(failed_ean_list, ean_no)
    }
    temp <- gsub(pattern = "[^,\\(]+\\(", replacement = "", temp)
    temp <- gsub(comma_replacements, replacement = ",", temp, ignore.case = T)
    temp %<>% gsub(pattern = "\\(|\\)", replacement = ",")
    temp <- gsub("\\.|;|:", ",", temp)
    #browser()
    
    #browser()
    temp <- gsub(pattern = "'(.*)'", replacement = "\\1", temp, ignore.case = T)
    temp <- gsub(pattern = "'and'", replacement = 'and', temp, ignore.case = T)
    temp <- gsub(pattern = "'and/or'", replacement = 'and/or', temp, ignore.case = T)
    temp <- gsub(pattern = ",{2,}", replacement = ",", temp)
    temp <- gsub("\\*|\\*\\*", "", temp)
    temp <- gsub("\\+|\\+\\+", "", temp)
    
    
    #print(temp)
    ingd1_prep <- prepare_text(temp)
    #browser()
    #print(ean_no)
    func2_output <- ingid_list_prep_lvl2(brackets = ingd1_prep, ingrid_values = temp, braces, neg_braces, ean_no)
    #browser()
    rm(temp)
    #browser()
    list_ingid <- func2_output$list_of_ingrids
    #failed_ean_list <- failed_ean_list  ##Gets the list of EANs that failed from func2
    
    final_list <- bind_rows(final_list, list_ingid)
    
    
    #abort(print(ean_no))
    
  }
  
  failed_ean_list <- failed_ean_list[!is.null(failed_ean_list)]
  
  final2_list <- cbind(dataframe[[IDcol]][1:len_df], final_list)
  
  colnames(final2_list)[1] <- IDcol
  #View(final2_list %>% filter(IDcol %in% failed_ean_list))
  final2_list %<>% filter(!IDcol %in% failed_ean_list)
  
  
  func3_output <- list("combined_list" = as.data.frame(final2_list), "failed_ean" = failed_ean_list)
  return(func3_output)
}

##End of Ingredient Separator for level2


##Counting ingredients
ingrid_count <- function(dataFrame, updateProgress2 = NULL){
  #named_list <- data.frame(ingredient = NA, counts = NA, EANs = NA)
  named_list <- vector(mode = "list", length = 10000)
  #print(colnames(named_list))
  for (i in 1:nrow(dataFrame)) {
    
    if (is.function(updateProgress2)) {
      text <- paste0("Rows processed = ", i)
      updateProgress2(value = i, detail = text)
    }
    
    for (j in 2:ncol(dataFrame)) {
      
      if(!dataFrame[[i,j]] %in% (named_list$ingredient)){
        
        temp_val <- data.frame(ingredient = dataFrame[[i,j]], counts = 1, EANs = dataFrame[[i,1]])
        
        named_list <- bind_rows(named_list, temp_val)
        named_list %<>% na.omit()
        
      }else if(!is.na(dataFrame[[i,j]])){
        
        named_list$counts[named_list$ingredient == dataFrame[[i,j]]] <- sum(named_list$counts[named_list$ingredient == dataFrame[[i,j]]],
                                                                            1, na.rm = T)
        named_list$EANs[named_list$ingredient == dataFrame[[i,j]]] <- paste(named_list$EANs[named_list$ingredient == dataFrame[[i,j]]],
                                                                            dataFrame[[i,1]], sep = ",")
        
      }
      
    }
    
  }
  
  counts_ingredients <- named_list %>% as.data.frame() %>% na.omit()# %>% melt()
  #counts_ingredients$ingredient <- names(named_list)
  return(counts_ingredients)
}
#####################



########################Added 06_24_2020
##ingredient parse for level n
func_remove_n_bracket <- function(text,level= 1){
  
  #i = 1
  #level = 5
  braces <- 1
  names(braces) <- "("
  neg_braces <- -1
  names(neg_braces) <- ")"
  
  #browser()
  while(level !=0) {
    list_of_ingrids <- vector(mode = "list", length = 10000)
    #i = 1
    j = 1
    brackets <- prepare_text(text)
    i = which(names(brackets) == "(")[1]
    if(is.na(i)) {
      text = ""
      break
    }
    while(i != length(brackets)) {
      #browser()
      counter <- braces[names(brackets)[i]]
      #print(counter)
      if(is.na(counter)){
        break
      }
      k = brackets[i] + 1
      #browser()
      while(counter != 0){   ##run loop till matching bracket found
        
        i= i+1
        counter <- sum(braces[names(brackets)[i]],neg_braces[names(brackets)[i]], counter, na.rm = T)
        if(i > length(brackets)) {
          #fails_list <- c(fails_list, ean_no)
          break()
        }
      }
      list_of_ingrids[[j]] <- substr(text, k, brackets[i] - 1) %>% tolower()
      # list_of_ingrids[[j]] <- ifelse(substr(text, k, brackets[i] - 1) %>%
      #                                  grepl(pattern = "\\([^,]+,.*\\)"), 
      #                                substr(text, k, brackets[i] - 1), 
      #                                "")
      # #k = brackets[i] + 1
      #browser()
      while(names(brackets[i]) != "(" & i != length(brackets)){
        i = i + 1
      }
      j = j + 1
      
    }
    #browser()
    temp_text <- unlist(list_of_ingrids)
    
    text <- temp_text[grepl(pattern = ",", temp_text)]
    level = level - 1
    text[lapply(text,length)>0] 
    if(length(text) == 0){
      text = ""
      break
    }
  }
  #(print(text))
  return(text)
}
