##
#source("database.R")
source("libraries.R")

## Custom Functions


prepare_text <-
  function(ingrid_values) {
    ##This function finds out the position of all the brackets and commas
    
    #Need to know whether the braces have any comma in between them
    ##For comma
    comma_list1 <- gregexpr(",", ingrid_values)[[1]]
    names(comma_list1) <- rep(",", length(comma_list1))
    
    ##For open round brackets
    round_openb_list <- gregexpr("\\(", ingrid_values, perl = T)[[1]]
    names(round_openb_list) <- rep("(", length(round_openb_list))
    
    ##For close round brackets
    round_closeb_list <- gregexpr("\\)", ingrid_values, perl = T)[[1]]
    names(round_closeb_list) <- rep(")", length(round_closeb_list))
    
    ##appending final list
    brackets <-
      c(comma_list1,
        round_openb_list,
        round_closeb_list,
        str_length(ingrid_values))
    brackets %<>% sort(decreasing = F)
    brackets <-
      brackets[brackets > 0] ##Ensuring that the characters not found are removed from the analysis
    
    
    return(brackets)
    rm(brackets)
  }
############################

ingid_list_prep <-
  function(brackets,
           ingrid_values,
           braces,
           neg_braces,
           ean_no) {
    ##Based on the results of the "prepare_text" function, this function separates the ingredients
    list_of_ingrids <- list()
    i = 1
    k = 1
    
    
    ##Ingredient replacement and comma replacement should be applied to both Clubbed ingredients and non-brackets.
    
    ##reading the CLUBBED INGREDIENTS file
    
    
    sub_ingred <-
      gregexpr(pattern = club_ingred,
               text = ingrid_values,
               ignore.case = T)
    #print(sub_ingred[[1]][1])
    #browser()
    if (sub_ingred[[1]][1] != -1) {
      ##A match is found
      ingrid_values <-
        gsub(
          ingrid_values,
          pattern = club_ingred,
          replacement = "",
          ignore.case = T,
          perl = T
        )
      #print("found_it")
      #browser()
      p = 0
      for (sub_ingreds in 1:length(sub_ingred[[1]])) {
        matched = sub_ingred[[1]][sub_ingreds] + attr(sub_ingred[[1]], 'match.length')[sub_ingreds] + p
        
        brackets <- brackets[-match(matched - 1, brackets)]
        brackets[which(brackets > matched)] <-
          brackets[which(brackets > matched)] - attr(sub_ingred[[1]], 'match.length')[sub_ingreds]
        p = p - attr(sub_ingred[[1]], 'match.length')[sub_ingreds]
      }
      
    }
    

    while (i <= length(brackets)) {
      if (!names(brackets)[i] %in% names(braces)) {
        ##the ingredients are not within parenthesis

        
        
        list_of_ingrids[[i]] <-
          substr(ingrid_values, k, brackets[i] - 1) %>% trimws() %>% tolower()
        
      } else {

        while (i <= length(brackets) & names(brackets[i]) != ",") {
          if (names(brackets[i]) == "(") {
            counter <- braces[names(brackets)[i]]
            while (counter != 0) {
              ##run loop till matching bracket found
              i = i + 1
              counter <-
                sum(braces[names(brackets)[i]], neg_braces[names(brackets)[i]], counter, na.rm = T)
              if (i > length(brackets)) {
                #fails_list <- c(fails_list, ean_no)
                break()
              }
            }
          }
          i = i + 1
        }
        list_of_ingrids[[i]] <-
          substr(ingrid_values, k, brackets[i] - 1) %>% trimws() %>% tolower()
      } ##End of modification
      if (names(brackets)[i] %in% names(neg_braces)) {
        names(brackets[i]) <- ","
        substr(ingrid_values, brackets[i], brackets[i]) <- ","
      }
      k = brackets[i] + 1
      i = i + 1
    }
    
    #########
    
    list_of_ingrids[sapply(list_of_ingrids, is.null)] <- NULL
    list_of_ingrids[list_of_ingrids == ""] <- NULL
    if (length(list_of_ingrids) == 0) {
      #browser()
      list_of_ingrids[1] <- NA
    }
    names(list_of_ingrids) <-
      paste0("ingredient", seq(1, length(list_of_ingrids)))
    func2_output <- list("list_of_ingrids" = list_of_ingrids)
    #return(list_of_ingrids)
    #abort(print(ean_no))
    return(func2_output)
    rm(list_of_ingrids)
    
  }


###########################
ingid_list_prep_lvl2 <-
  function(brackets,
           ingrid_values,
           braces,
           neg_braces,
           ean_no) {
    ##Based on the results of the "prepare_text" function, this function separates the ingredients
    list_of_ingrids <- list()
    
    
    ##Separating ingredients
    ingrid_values %<>% gsub(pattern = "\\(|\\)", replacement = ",")
    list_of_ingrids <-
      str_split(ingrid_values, pattern = ",")[[1]] %>% trimws() %>% as.list()
    
    
    
    
    
    #########

    list_of_ingrids[sapply(list_of_ingrids, is.null)] <- NULL
    list_of_ingrids[list_of_ingrids == ""] <- NULL
    if (length(list_of_ingrids) == 0) {
      list_of_ingrids[1] <- NA
    }
    #list_of_ingrids <- ifelse(length(list_of_ingrids) == 0, list_of_ingrids[1] <- NA, list_of_ingrids)
    names(list_of_ingrids) <-
      paste0("ingredient", seq(1, length(list_of_ingrids)))

    func2_output <- list("list_of_ingrids" = list_of_ingrids)

    return(func2_output)
    rm(list_of_ingrids)
    
  }

### End of ingredient list prep level 2
#failed_ean_list <- vector(mode = "list", length = 0)
failed_ean_list <- c()
#percComplete <- 0
## Ingredient separate - function###############################
func_ingredTransform <- function(text, ean_no) {
  braces <- c(1,1,1)
  names(braces) <- c("(", "{", "[")
  neg_braces <- c(-1, -1, -1)
  names(neg_braces) <- c(")", "}", "]")
  text <- gsub("\\{|\\[", replacement = "(", text)
  text <- gsub("\\}|\\]", replacement = ")", text)
  #text <- gsub(pattern = ", \\(", replacement = " \\(", text)
  #text <- gsub(pattern = ",\\(", replacement = " \\(", text)
  text <- gsub("\\*|\\*\\*", "", text)
  text <- gsub("\\+|\\+\\+", "", text)
  for (ing_repl in 1:nrow(ingred_replacements)) {
    #browser()
    #print(without_brackets)
    text <- gsub(
      pattern = as.character(ingred_replacements$Ingredient[ing_repl]),
      replacement = as.character(ingred_replacements$Replacement[ing_repl]),
      text,
      ignore.case = T
    )

  }
  
  text <-
    gsub(
      pattern = "\\sand\\s(\\w*$)",
      replacement = ",\\1",
      ignore.case = T,
      text
    )   ##Replaces and with ,(comma) if the pattern is ' word and word[end]'
  text <-
    gsub(
      pattern = "\\sand\\s(\\w*\\.$)",
      replacement = ",\\1",
      ignore.case = T,
      text
    )##Replaces and with ,(comma) if the pattern is ' word and word.[end]'
  
  text <-
    paste0(text, ifelse(grepl(pattern = "\\.$", x = text), "", "."), sep = "")  ###Adds a "." at the end of the description of each food
  text <- gsub("\\.|;", ",", text)
  
  ingd0_prep <- prepare_text(text)
  
  ## Fails List: if the number of opening and closing brackets don't match add it to fails_list
  failTag <- 0
  if (sum(braces[names(ingd0_prep)], neg_braces[names(ingd0_prep)], na.rm = T) != 0) {
    #failed_ean_list <<- c(failed_ean_list, ean_no)
    failTag <- 1
  }

  
  sub_ingred <-
    gregexpr(pattern = club_ingred,
             text = text,
             ignore.case = T)
  
  if (sub_ingred[[1]][1] != -1) {
    ##A match is found
    text <-
      gsub(
        text,
        pattern = club_ingred,
        replacement = "",
        ignore.case = T
      )
    #print("found_it")
    #browser()
    p = 0
    for (sub_ingreds in 1:length(sub_ingred[[1]])) {
      matched = sub_ingred[[1]][sub_ingreds] + attr(sub_ingred[[1]], 'match.length')[sub_ingreds] + p
      
      ingd0_prep <- ingd0_prep[-match(matched - 1, ingd0_prep)]
      ingd0_prep[which(ingd0_prep > matched)] <-
        ingd0_prep[which(ingd0_prep > matched)] - attr(sub_ingred[[1]], 'match.length')[sub_ingreds]
      p = p - attr(sub_ingred[[1]], 'match.length')[sub_ingreds]
    }
    
  }
  
  k = 1
  i = 1
  without_brackets <- list()
  with_brackets <- list()
  while (i <= length(ingd0_prep)) {
    if (!names(ingd0_prep)[i] %in% names(braces)) {
      ##the ingredients are not within parenthesis

      without_brackets <-
        append(without_brackets,
               substr(text, k, ingd0_prep[i] - 1) %>%
                 #trimws() %>%
                 tolower())
      
    }
    
    
    else if (names(ingd0_prep)[i] %in% names(braces)) {
      #browser()
      ##New changes 12/30
      outside_braces1 <-
        substr(text, k, ingd0_prep[i] - 1) #%>% trimws() %>%  tolower()
      outside_braces <- outside_braces1
      len_out_braces <- nchar(outside_braces)

      outside_braces <-
        gsub("\\.|;", ",", outside_braces)     ##Changed: commented on 5/28/2020
      outside_braces <- gsub("\\*|\\*\\*", "", outside_braces)
      outside_braces1 <- gsub("\\*|\\*\\*", "", outside_braces1)
      outside_braces1 <- gsub("\\+|\\+\\+", "", outside_braces1)
      outside_braces <- gsub("\\+|\\+\\+", "", outside_braces)
      outside_braces <-
        gsub(
          comma_replacements,
          replacement = ",",
          outside_braces,
          ignore.case = T
        )
      ##New K is the last comma in outside_braces + 1

      text <-
        sub(pattern = outside_braces1, replacement = outside_braces, text)
      last_comma <-
        gregexpr(pattern = ",", outside_braces)[[1]][length(gregexpr(pattern = ",", outside_braces)[[1]])]
      k <- sum(c(k, last_comma), na.rm = T)
      without_brackets <-
        append(without_brackets,
               substr(outside_braces, 0, last_comma - 1))

      ##New Changes 07_17_2020
      #browser()
      while (i <= length(ingd0_prep) & names(ingd0_prep[i]) != ",") {
        if (names(ingd0_prep[i]) == "(") {
          counter2 <- braces[names(ingd0_prep)[i]]
          while (counter2 != 0) {
            ##run loop till matching bracket found
            i = i + 1
            counter2 <-
              sum(braces[names(ingd0_prep)[i]], neg_braces[names(ingd0_prep)[i]], counter2, na.rm = T)
            if (i > length(ingd0_prep)) {
              #fails_list <- c(fails_list, ean_no)
              break()
            }
          }
        }
        i = i + 1
      }
      ##End of New Changes
      ##End of addition
      ##Added 1/6/2020 - to remove extra spaces being put into the with_brackets terms

      ingd0_prep <-
        ingd0_prep + (nchar(outside_braces) - nchar(outside_braces1))
      with_brackets <-
        append(
          with_brackets,
          substr(text, k, ingd0_prep[i] - 1) %>% gsub(pattern = "\\s{2,}", replacement = " ") %>% trimws() %>% tolower()
        )
      ##with_brackets <- append(with_brackets, substr(text, k, ingd0_prep[i]) %>% trimws() %>% tolower()) ##Original code
      ##End of addition
      
    } else if (names(ingd0_prep)[i] %in% names(neg_braces)) {
      names(ingd0_prep[i]) <- ","
      substr(text, ingd0_prep[i], ingd0_prep[i]) <- ","
    }
    k = ingd0_prep[i] + 1
    i = i + 1
  }
  #browser()
  without_brackets <-
    paste0(na.omit(without_brackets), collapse = ",")

  #browser()
  without_brackets <-
    gsub("\\.|;", ",", without_brackets)    ##Changed: removed ":" on 5/28/2020
  without_brackets <-
    gsub(
      comma_replacements,
      replacement = ",",
      without_brackets,
      ignore.case = T
    )
  #without_brackets <- gsub(pattern = "'and'", replacement = 'and', without_brackets, ignore.case = T)
  with_brackets <- paste0(na.omit(with_brackets), collapse = ",")
  #with_brackets <- gsub(pattern = "'and'", replacement = 'and', with_brackets, ignore.case = T)
  text <- paste(without_brackets, with_brackets, sep = ",")
  text <-
    paste0(text, ifelse(grepl(pattern = "\\.$", x = text), "", "."), sep = "")  ###Adds a "." at the end of the description of each food
  text <-
    gsub("\\.|;|:", ",", text)  ##Changed: removed ":" on 5/28/2020
  #browser()
  
  #browser()
  text <-
    gsub(
      pattern = "'and'",
      replacement = 'and',
      text,
      ignore.case = T
    )
  
  text <-
    gsub(
      pattern = "'&'",
      replacement = '&',
      text,
      ignore.case = T
    )
  
  text <-
    gsub(
      pattern = "'and/or'",
      replacement = 'and/or',
      text,
      ignore.case = T
    )
  text <- gsub(pattern = ",{2,}", replacement = ",", text)
  text <- gsub("\\*|\\*\\*", "", text)
  text <- gsub("\\+|\\+\\+", "", text)
  rm(without_brackets)
  rm(with_brackets)
  
  #print(text)
  ingd1_prep <- prepare_text(text)
  #browser()
  
  func2_output <-
    ingid_list_prep(brackets = ingd1_prep,
                    ingrid_values = text,
                    braces,
                    neg_braces,
                    ean_no)
  rm(text)
  list_ingid <- func2_output$list_of_ingrids
  
  if (failTag == 1) {
    list_ingid <- append(list_ingid, c("failed" = T))
  } else {
    list_ingid <- append(list_ingid, c("failed" = F))
  }

  
  return(list_ingid)
}

#### End of Function#################################################


ingrid_sep <-
  function(dataframe, IDcol, Valcol, updateProgress = NULL) {
    ##This function combines the ingredient list from all the foods (_main_ function)
    #dataframe %<>% mutate_if(!is.data.frame, as.data.frame)

    #failed_ean_list <- c()
    cluster <- makeCluster(detectCores() - 1)
    
    clusterExport(cluster, list("prepare_text", "ingred_replacements", "str_length", "%<>%",
                                "club_ingred", "%>%", "comma_replacements",
                                "ingid_list_prep"))
    

    
    final2_list <- dataframe[,1] %>% bind_cols(
      parApply(cl = cluster, X = as.data.frame(dataframe[,2]), MARGIN = 1, FUN = func_ingredTransform) %>%
        bind_rows() %>% as.data.frame()
      ## Serialized 
      # apply(X = as.data.frame(dataframe[,2]), MARGIN = 1, FUN = func_ingredTransform) %>%
      #   bind_rows() %>% as.data.frame()
    )

    
    
    
      
    stopCluster(cluster)
    
    colnames(final2_list)[1] <- IDcol
    
    failed_ean_list <- final2_list %>% filter(failed) %>% select(EAN) %>% as.vector()
    
    final2_list %<>% filter(!failed) %>% select(-failed)

    func3_output <-
      list("combined_list" = as.data.frame(final2_list),
           "failed_ean" = failed_ean_list)
    return(func3_output)
  }


####ingredient separator for level 2
ingrid_sep_lvl_2 <-
  function(dataframe, IDcol, Valcol, updateProgress = NULL) {
    ##This function combines the ingredient list from all the foods (_main_ function)
    
    #dataframe <- gsub("\\{|\\[", replacement = "(", dataframe)
    
    braces <- c(1, 1, 1)
    names(braces) <- c("(", "{", "[")
    neg_braces <- c(-1,-1,-1)
    names(neg_braces) <- c(")", "}", "]")
    
    len_df <- nrow(dataframe)
    
    final_list <- data.frame()
    failed_ean_list <- c()
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

      temp <-
        gsub(
          pattern = "\\sand\\s(\\w*$)",
          replacement = ",\\1",
          ignore.case = T,
          temp
        )   ##Replaces 'and' with ,(comma) if the pattern is ' word and word[end]'
      temp <-
        gsub(
          pattern = "\\sand\\s(\\w*\\.$)",
          replacement = ",\\1",
          ignore.case = T,
          temp
        )##Replaces 'and' with ,(comma) if the pattern is ' word and word.[end]'
      temp <-
        gsub(
          pattern = "[^\\(,]+\\([^\\),]+\\)",
          replacement = "",
          ignore.case = T,
          temp
        )  ##Removes all brackets
      
      temp <-
        paste0(temp, ifelse(grepl(pattern = "\\.$", x = temp), "", "."), sep = "")  ###Adds a "." at the end of the description of each food
      temp <- gsub("\\.|;|:", ",", temp)
      ingd0_prep <- prepare_text(temp)
      ### Fails List: if the number of opening and closing brackets don't match add it to fails_list
      if (sum(braces[names(ingd0_prep)], neg_braces[names(ingd0_prep)], na.rm = T) != 0) {
        failed_ean_list <- c(failed_ean_list, ean_no)
      }
      temp <- gsub(pattern = "[^,\\(]+\\(", replacement = "", temp)
      temp <-
        gsub(comma_replacements,
             replacement = ",",
             temp,
             ignore.case = T)
      temp %<>% gsub(pattern = "\\(|\\)", replacement = ",")
      temp <- gsub("\\.|;|:", ",", temp)
      #browser()
      
      #browser()
      temp <-
        gsub(
          pattern = "'(.*)'",
          replacement = "\\1",
          temp,
          ignore.case = T
        )
      temp <-
        gsub(
          pattern = "'and'",
          replacement = 'and',
          temp,
          ignore.case = T
        )
      temp <-
        gsub(
          pattern = "'and/or'",
          replacement = 'and/or',
          temp,
          ignore.case = T
        )
      temp <- gsub(pattern = ",{2,}", replacement = ",", temp)
      temp <- gsub("\\*|\\*\\*", "", temp)
      temp <- gsub("\\+|\\+\\+", "", temp)
      
      
      #print(temp)
      ingd1_prep <- prepare_text(temp)
      #browser()
      #print(ean_no)
      func2_output <-
        ingid_list_prep_lvl2(brackets = ingd1_prep,
                             ingrid_values = temp,
                             braces,
                             neg_braces,
                             ean_no)
      #browser()
      rm(temp)
      #browser()
      list_ingid <- func2_output$list_of_ingrids
      #failed_ean_list <- failed_ean_list  ##Gets the list of EANs that failed from func2
      
      final_list <- bind_rows(final_list, list_ingid)

      
    }
    
    failed_ean_list <- failed_ean_list[!is.null(failed_ean_list)]
    
    final2_list <- cbind(dataframe[[IDcol]][1:len_df], final_list)
    
    colnames(final2_list)[1] <- IDcol
    final2_list %<>% filter(!IDcol %in% failed_ean_list)
    
    
    func3_output <-
      list("combined_list" = as.data.frame(final2_list),
           "failed_ean" = failed_ean_list)
    return(func3_output)
  }

##End of Ingredient Separator for level2


##Counting ingredients
ingrid_count <- function(dataFrame, updateProgress2 = NULL) {
  #named_list <- data.frame(ingredient = NA, counts = NA, EANs = NA)
  named_list <- vector(mode = "list", length = 10000)
  #print(colnames(named_list))
  for (i in 1:nrow(dataFrame)) {
    if (is.function(updateProgress2)) {
      text <- paste0("Rows processed = ", i)
      updateProgress2(value = i, detail = text)
    }
    
    for (j in 2:ncol(dataFrame)) {
      if (!dataFrame[[i, j]] %in% (named_list$ingredient)) {
        temp_val <-
          data.frame(ingredient = dataFrame[[i, j]],
                     counts = 1,
                     EANs = dataFrame[[i, 1]])
        
        named_list <- bind_rows(named_list, temp_val)
        named_list %<>% na.omit()
        
      } else if (!is.na(dataFrame[[i, j]])) {
        named_list$counts[named_list$ingredient == dataFrame[[i, j]]] <-
          sum(named_list$counts[named_list$ingredient == dataFrame[[i, j]]],
              1, na.rm = T)
        named_list$EANs[named_list$ingredient == dataFrame[[i, j]]] <-
          paste(named_list$EANs[named_list$ingredient == dataFrame[[i, j]]],
                dataFrame[[i, 1]], sep = ",")
        
      }
      
    }
    
  }
  
  counts_ingredients <-
    named_list %>% as.data.frame() %>% na.omit()# %>% melt()
  #counts_ingredients$ingredient <- names(named_list)
  return(counts_ingredients)
}
#####################



########################Added 06_24_2020
##ingredient parse for level n
func_remove_n_bracket <- function(text, level = 1) {
  #i = 1
  #level = 5
  braces <- 1
  names(braces) <- "("
  neg_braces <- -1
  names(neg_braces) <- ")"
  
  #browser()
  while (level != 0) {
    list_of_ingrids <- vector(mode = "list", length = 10000)
    #i = 1
    j = 1
    brackets <- prepare_text(text)
    i = which(names(brackets) == "(")[1]
    if (is.na(i)) {
      text = ""
      break
    }
    while (i != length(brackets)) {
      #browser()
      counter <- braces[names(brackets)[i]]
      #print(counter)
      if (is.na(counter)) {
        break
      }
      k = brackets[i] + 1
      #browser()
      while (counter != 0) {
        ##run loop till matching bracket found
        
        i = i + 1
        counter <-
          sum(braces[names(brackets)[i]], neg_braces[names(brackets)[i]], counter, na.rm = T)
        if (i > length(brackets)) {
          #fails_list <- c(fails_list, ean_no)
          break()
        }
      }
      list_of_ingrids[[j]] <-
        substr(text, k, brackets[i] - 1) %>% tolower()

      while (names(brackets[i]) != "(" & i != length(brackets)) {
        i = i + 1
      }
      j = j + 1
      
    }
    #browser()
    temp_text <- unlist(list_of_ingrids)
    
    text <- temp_text[grepl(pattern = ",", temp_text)]
    level = level - 1
    text[lapply(text, length) > 0]
    if (length(text) == 0) {
      text = ""
      break
    }
  }
  #(print(text))
  return(text)
}
