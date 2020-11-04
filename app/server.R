#print(getwd())

source("libraries.R")
#source("database.R")
source("readFiles.R")
source("functions_parallel.R")
#source("..//R-scripts//functions_parallel.R")



server <- function(input, output, session) {
    ingrid_input <-
        eventReactive(input$ingrid_file$datapath, {
            ##Reactive Conductor
            data.ingrid = read_xlsx(path = input$ingrid_file$datapath) %>% na.omit()
            return(data.ingrid)
        }) ##Reads the ingredient PlayerValue
    #output$file_read <- renderDT(ingred_replacements)#, " lines of ingredient replacements.")
    #print(paste0("Read in ", nrow(ingred_replacements), " lines of ingredient replacements."))
    
    
    output$table1 <-
        DT::renderDataTable(
            expr =  ingrid_input(),
            server = T,
            editable = T,
            style = "bootstrap",
            escape = F,
            extensions = 'FixedHeader',
            options = list(escape = F),
            colnames = c("EAN number", "Ingredients"),
            caption = HTML('<b>Uploaded ingredient file</b>')
        ) ###Displaying the data
    
    
    
    ##process on button click
    ##Giving temp - reactiveValues
    default_table <- reactiveValues(data = NULL)
    default_comp_ingrid <- reactiveValues(temp = NULL)
    default_comp_ingrid_temp <- reactiveValues(temp2 = NULL)
    
    
    observeEvent(input$process0, {
        ###The start of processing
        
        
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data; WAIT UNTIL this notification closes before progressing further")
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        
        # updateProgress <- function(value = NULL, detail = NULL) {
        #     progress$inc(amount = 1/nrow(ingrid_input()), detail = detail)
        # }
        
        
        
        func3_output <-
            ingrid_sep(ingrid_input(), "EAN", "ingredients_english")#, updateProgress) ##Removed for parallelization
        default_comp_ingrid$temp <-
            func3_output$combined_list %>% as.data.frame() %>% filter(!EAN %in% as.list(func3_output$failed_ean))
        
        
        
        failed_ean_func3 <-
            ingrid_input() %>% filter(EAN %in% (func3_output$failed_ean[[1]]))
        default_table$data <- failed_ean_func3
        
        
    })
    
    observeEvent(input$process1, {
        ###Update the ingredient values based on the input from the user
        func3_output2 <-
            ingrid_sep(default_table$data, "EAN", "ingredients_english")
        default_comp_ingrid_temp$temp2 <-
            func3_output2$combined_list %>% as.data.frame() %>% filter(!EAN %in% as.list(func3_output2$failed_ean))
        
        #_______
        failed_ean_func3_2 <-
            default_table$data %>% filter(EAN %in% as.list(func3_output2$failed_ean))
        default_table$data <- failed_ean_func3_2
        
    })
    ##download raw file with corrected brackets
    # output$downloadData3 <- downloadHandler(
    #     filename = function() {
    #         paste(input$file_name_ingrid, ".xlsx")
    #     },
    #     content = function(file) {
    #         #write.csv(ingrid_counts_list$data, file, row.names = FALSE)
    #         write_xlsx(default_comp_ingrid$temp, path = file)
    #     }
    #
    # )
    
    ###Added, was separately outside this
    
    output$table2 <- DT::renderDT(
        default_table$data,
        server = T,
        editable = T,
        #style = "bootstrap",
        selection = list(mode = "single"),
        caption = HTML(
            '<b>Add missing brackets at appropriate locations in the list below by double clicking on the "Ingredients" column and then click (3)Update. OR, make the changes in a copy of the raw file and re-upload the corrected version </b>'
        ),
        options = list(searchHighlight = TRUE),
        colnames = c("EAN number", "Ingredients")
    )
    
    #####Edit based on user input
    proxy = dataTableProxy('table2')
    observeEvent(input$table2_cell_edit, {
        info = input$table2_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        default_table$data[i, j] <<-
            DT::coerceValue(v, default_table$data[i, j])
        #output$t1 <- renderPrint(default_table$data[i,j])
        replaceData(proxy, default_table$data, resetPaging = FALSE)  # important
    })
    
    
    
    observeEvent(input$process1, {
        default_comp_ingrid$temp <-
            bind_rows(default_comp_ingrid$temp,
                      default_comp_ingrid_temp$temp2)
    })
    # output$downloadBrackets_corrected <- downloadHandler(
    #   filename = function() {
    #     paste(input$file_name_brackets, ".xlsx")
    #   },
    #   content = function(file) {
    #     #write.csv(ingrid_counts_list$data, file, row.names = FALSE)
    #     write_xlsx(default_comp_ingrid$temp, path = file)
    #   }
    # )
    
    #output$table3 <- renderDT(default_comp_ingrid$temp)  ##Has been disabled
    
    
    ##table4
    ingrid_counts_list <- reactiveValues(data = NULL)
    
    # Downloadable csv of selected dataset ----
    output$downloadData1 <- downloadHandler(
        filename = function() {
            paste(input$file_name_ingrid, ".xlsx")
        },
        content = function(file) {
            #write.csv(ingrid_counts_list$data, file, row.names = FALSE)
            write_xlsx(default_comp_ingrid$temp, path = file)
        }
        
    )
    output$downloadData2 <- downloadHandler(
        filename = function() {
            paste(input$file_name_ingrid_count, ".xlsx")
        },
        content = function(file) {
            #write.csv(ingrid_counts_list$data, file, row.names = FALSE)
            write_xlsx(ingrid_counts_list$data, path = file)
        }
    )
    ###Download data for level 2 2_18_2020
    output$downloadData1_lvl2 <- downloadHandler(
        filename = function() {
            paste(input$file_name_ingrid_lvl2, ".xlsx")
        },
        content = function(file) {
            #write.csv(ingrid_counts_list$data, file, row.names = FALSE)
            write_xlsx(lvl2_ingredients_sep$data$combined_list, path = file)
        }
        
    )
    output$downloadData2_lvl2 <- downloadHandler(
        filename = function() {
            paste(input$file_name_ingrid_count_lvl2, ".xlsx")
        },
        content = function(file) {
            #write.csv(ingrid_counts_list$data, file, row.names = FALSE)
            #browser()
            write_xlsx(ingrid_counts_list_lvl2$data, path = file)
        }
    )
    ##End of Addition
    
    
    
    
    
    #parent_name_table <- reactiveValues(data = NULL)
    #parent_name_table$data <- read.csv("parent names.csv", stringsAsFactors = F)
    parent_name_table_file <-
        reactiveFileReader(
            intervalMillis = 500,
            session = session,
            filePath  = thesaurusFilePath,
            readFunc = read.csv,
            stringsAsFactors = F,
            encoding = "UTF-8"
        )
    output$table_parents <- renderDT(parent_name_table_file())
    
    new_parents <- reactiveValues(data = NULL)
    observeEvent(input$count_compute2, {
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        
        updateProgress2 <- function(value = NULL,
                                    detail = NULL) {
            progress$inc(amount = 1 / nrow(default_comp_ingrid$temp),
                         detail = detail)
        }
        
        ingrid_counts_list$data <-
            ingrid_count(default_comp_ingrid$temp, updateProgress2)
        ingrid_counts_list$data %<>% filter(ingredient != "") %>% arrange(counts, ingredient) %>%
            mutate(ingredient = as.character(ingredient))
        ##Showing parent name table along with the new computed table
        
        
        
        new_parents <- reactiveValues(data = NULL)
        observeEvent(input$showAllIngredients1, ignoreNULL = FALSE, {
            if (input$showAllIngredients1) {
                new_parents$data <- ingrid_counts_list$data
            } else {
                new_parents$data <-
                    ingrid_counts_list$data[!ingrid_counts_list$data$ingredient %in% tolower(parent_name_table_file()$ingredient), ]
            }
        })
        
        
        #View(new_parents$data)
        
        ###
        output$table6 <- DT::renderDataTable(
            new_parents$data,
            editable = T,
            server = T,
            options = list(
                searchHighlight = TRUE,
                
                columnDefs = list(
                    list(
                        targets = 3,
                        className = 'dt-center',
                        targets = 5,
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 13 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 13) + '...</span>' : data;",
                            "}"
                        )
                    )
                )
            ),
            caption = HTML(
                '<b>Table 1: Ingredient list extracted from the uploaded data</b>'
            ),
            style = "bootstrap",
            extensions = 'FixedHeader',
            colnames = c("Sl No", "Ingredients", "counts", "EAN")
        )
        
        ##table6 is editable
        proxy = dataTableProxy('table6')
        observeEvent(input$table6_cell_edit, {
            info = input$table6_cell_edit
            str(info)
            i = info$row
            j = info$col
            v = info$value
            default_comp_ingrid$temp[default_comp_ingrid$temp == as.character(new_parents$data[i, j])] <-
                v
            
            ingrid_counts_list$data[ingrid_counts_list$data == as.character(new_parents$data[i, j])] <-
                v
            
            ####
            new_parents$data <-
                ingrid_counts_list$data[!ingrid_counts_list$data$ingredient %in% tolower(parent_name_table_file()$ingredient), ]
            ######
            
            
            new_parents$data %<>% group_by(ingredient) %>% mutate(counts = sum(counts)) %>% unique() %>% ungroup()
            
            replaceData(proxy, new_parents$data, resetPaging = FALSE)
            # important
        })
        
        
        
        
        
        output$table7 <-
            DT::renderDataTable(
                parent_name_table_file(),
                options = list(
                    searchHighlight = TRUE,
                    ageLength = 5,
                    autoWidth = TRUE
                ),
                selection = list(mode = "single"),
                caption = HTML(
                    '<b>Table 2: Ingredient Thesaurus: Only the "Most common name" would be selected for assignment</b>'
                ),
                style = "bootstrap",
                colnames = c(
                    "Most common name",
                    "Other names",
                    "TimeAdded",
                    "PersonAdded"
                )
            )
        output$select_rows <-
            renderPrint(new_parents$data[input$table6_rows_selected, "ingredient"])
        
        
        sorted_thesaurus <- reactiveValues(data = NULL)
        observeEvent(input$table6_rows_selected, {
            #search = list()
            
            output$table7 <-
                DT::renderDataTable(
                    parent_name_table_file(),
                    selection = list(mode =
                                         "single"),
                    caption = HTML(
                        '<b>Table 2: Ingredient Thesaurus: Only the "Most common name" would be selected for assignment</b>'
                    ),
                    style = "bootstrap",
                    colnames = c(
                        "Most common name",
                        "Other names",
                        "TimeAdded",
                        "PersonAdded"
                    ),
                    options = list(
                        searchHighlight = TRUE,
                        search = list(search = sub(
                            "(\\w)\\s.*", "\\1", new_parents$data[c(input$table6_rows_selected)[1], "ingredient"]
                        ))
                    )
                )
            
            ##New addition 12/17 for fuzzy match
            if (input$fuzzy_match) {
                #print(input$fuzzy_match)
                #browser()
                search_term <-
                    new_parents$data[c(input$table6_rows_selected)[1], "ingredient"]
                sorted_thesaurus2 <-
                    parent_name_table_file() %>% select(parent_name) %>% unique()
                similarity_score <-
                    data.frame(
                        sim_score = adist(
                            search_term,
                            sorted_thesaurus2$parent_name,
                            partial = F,
                            ignore.case = T
                        ) %>% t()
                    )
                sorted_thesaurus2 <-
                    bind_cols(sorted_thesaurus2, similarity_score)
                
                sorted_thesaurus$data <- sorted_thesaurus2
                
                
                
                output$table7 <-
                    DT::renderDataTable(
                        sorted_thesaurus$data,
                        selection = list(mode =
                                             "single"),
                        caption = HTML(
                            '<b>Table 2: Ingredient Thesaurus: Only the "Most common name" would be selected for assignment</b>'
                        ),
                        style = "bootstrap",
                        colnames = c("Most common name", "Similarity"),
                        options = list(
                            searchHighlight = TRUE,
                            order = list(list(2, 'asc'))
                        )
                    )
            }
            
            
        })
        
        observeEvent(input$assign_parent, {
            if (is.null(input$table7_rows_selected) |
                is.null(input$table6_rows_selected)) {
                createAlert(
                    session = session,
                    "no_parent_alert",
                    title = "Select at least one entry from both tables",
                    style = "error",
                    append = F
                )
            } else {
                #browser()
                add_child <-
                    data.frame(
                        parent_name = sorted_thesaurus$data[input$table7_rows_selected, "parent_name"],
                        ingredient = c(toupper(new_parents$data[input$table6_rows_selected, "ingredient"])),
                        TimeAdded = as.character(Sys.time()),
                        PersonAdded = Sys.info()[8]
                    )
                
                #View(add_child)
                
                proxy = dataTableProxy('table6')
                new_parents$data <-
                    new_parents$data[-input$table6_rows_selected, ] %>% arrange(counts, ingredient)
                replaceData(proxy, new_parents$data, resetPaging = FALSE)
                
                
                
                parent_name_table_file2 <-
                    reactive({
                        rbind(parent_name_table_file(),
                              as.data.frame(add_child)) %>% group_by() %>% arrange(parent_name, ingredient) %>% ungroup()
                    })
                
                write.csv(
                    parent_name_table_file2(),
                    thesaurusFilePath,
                    row.names = F
                )
                # showModal(modalDialog(actionButton(inputId = "modify2", label = "Modify2 Description", icon = icon("multiply")),
                #                       title = "test", size = "l"))
            }
            
        })
        observeEvent(input$add_new_parent, {
            if (is.null(input$table6_rows_selected)) {
                createAlert(
                    session = session,
                    "new_parent_noname_alert",
                    title = "Select at least one entry from table 1",
                    style = "error",
                    append = F
                )
            } else {
                ######
                ###Add text input here####
                #browser()
                new_parent_name <-
                    ifelse(
                        is.na(input$parent_name) | input$parent_name == "" ,
                        toupper(new_parents$data[c(input$table6_rows_selected), "ingredient"]),
                        toupper(input$parent_name)
                    )
                #TimeAdded = Sys.time(),
                #PersonAdded = Sys.info()[8])
                ######
                add_parent <-
                    data.frame(
                        parent_name = new_parent_name,
                        ingredient = toupper(new_parents$data[c(input$table6_rows_selected), "ingredient"]),
                        TimeAdded = as.character(Sys.time()),
                        PersonAdded = Sys.info()[8]
                    )
                
                proxy = dataTableProxy('table6')
                new_parents$data <-
                    new_parents$data[-input$table6_rows_selected, ] %>% arrange(counts, ingredient)
                replaceData(proxy, new_parents$data, resetPaging = FALSE)
                
                parent_name_table_file2 <-
                    reactive({
                        rbind(parent_name_table_file(), add_parent) %>% group_by() %>% arrange(parent_name, ingredient) %>% ungroup()
                    })
                write.csv(
                    parent_name_table_file2(),
                    thesaurusFilePath,
                    row.names = F
                )
                updateTextInput(
                    session = session,
                    inputId = "parent_name",
                    label = "Add the most common name",
                    placeholder = "You can leave this blank",
                    value = NA
                )
            }
        })
        
        
        
        ##Add new parents
        
        ##Modify food
        ean_mods <- reactiveValues(eans = NULL)
        observeEvent(input$modify, {
            #print(input$modify)
            if (is.null(input$table6_rows_selected)) {
                createAlert(
                    session = session,
                    "food_select_modify_alert",
                    title = "Select at least one entry from table 1 to modify",
                    style = "error",
                    append = F
                )
            } else {
                #browser()
                mod_ean <-
                    new_parents$data[input$table6_rows_selected, "EANs"] %>% as.character()
                mod_ean <-
                    strsplit(mod_ean, split = ",")[[1]] %>% grep(pattern = "[0-9]+", value = T)
                ean_mods$eans <- mod_ean[1]
            }
        })
        
        observeEvent(input$modify, {
            #browser()
            updateTextAreaInput(
                session = session,
                inputId = "edit_food2",
                value = ingrid_input()$ingredients_english[ingrid_input()$EAN == ean_mods$eans],
                label = paste("EAN Number = ", ean_mods$eans)
            )
            
        })
        #print(input$edit_food2)
        observeEvent(input$modify_food_confirm, {
            temp_df <- data.frame(EAN = ean_mods$eans,
                                  ingredients_english = input$edit_food2)
            #View(temp_df)
            modified_food_parsed <-
                ingrid_sep(temp_df, "EAN", "ingredients_english")
            modified_food_failed <- modified_food_parsed$failed_ean
            if (is.null(modified_food_failed)) {
                modified_food_sep <- modified_food_parsed$combined_list
                #View(modified_food_sep)
                modified_food_count <-
                    ingrid_count(modified_food_sep)
                
                
                ingrid_counts_list$data %<>% mutate(
                    counts = ifelse(
                        grepl(pattern = ean_mods$eans, EANs),
                        counts - 1,
                        counts
                    ),
                    EANs = gsub(
                        pattern = ean_mods$eans,
                        replacement = "",
                        EANs
                    )
                ) %>%
                    filter(counts > 0) %>%
                    rbind(modified_food_count) %>% group_by(ingredient) %>%
                    mutate(
                        EANs = paste0(EANs, collapse = ",") %>% gsub(pattern = ",{2,}", replacement = ","),
                        counts = sum(counts)
                    ) %>% unique() %>% ungroup()
                
                
                new_parents$data <-
                    ingrid_counts_list$data[!tolower(ingrid_counts_list$data$ingredient) %in% tolower(parent_name_table_file()$ingredient), ]
                default_comp_ingrid$temp %<>% filter(EAN != ean_mods$eans) %>% bind_rows(modified_food_sep)
            } else{
                createAlert(
                    session = session,
                    anchorId = "mod_failed",
                    title = "Brackets inconsistent",
                    style = "error",
                    content = "There are incomplete brackets in the modified description: Dismiss this message by clicking on the cross",
                    dismiss = T,
                    append = F
                )
            }
            
        })
        
        
        
        observeEvent(input$omit_btn, {
            ingrid_counts_list$data %<>% mutate(ingredient = gsub(
                pattern = input$text_omit,
                ingredient,
                replacement = ""
            )) %>%
                filter(ingredient != "")
            new_parents$data <-
                ingrid_counts_list$data[!tolower(ingrid_counts_list$data$ingredient) %in% tolower(parent_name_table_file()$ingredient), ]
            #default_comp_ingrid$temp[default_comp_ingrid$temp == input$text_omit] <- NA %<>% filter(EAN != mod_ean) %>% bind_rows(modified_food_sep)
            omits_file <-
                rbind(data.frame(Omits = input$text_omit), omits_file)
            #View(omits_file)
            print("success")
            write_xlsx(omits_file, "omits_v1.xlsx")
        })
        
        
        
    })
    ###Input for level 1 parsed ingredient file ##Added 07_30_2020
    observeEvent(input$lvl1_parsed$datapath, ignoreNULL = TRUE, {
        default_comp_ingrid$temp <-
            read_xlsx(input$lvl1_parsed$datapath, sheet = 1)
    })
    ##End of addition 07_30_2020
    ###Level 2 Parsing Added on 02/13/2020
    lvl2_ingredients_sep <- reactiveValues(data = NULL)
    ingrid_counts_list_lvl2 <- reactiveValues(data = NULL)
    observeEvent(input$count_compute_lvl2, {
        lvl2_ingredients <- list()
        
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data; Please wait untill this notification closes", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        
        updateProgress3 <- function(value = NULL,
                                    detail = NULL) {
            progress$inc(amount = 1 / nrow(default_comp_ingrid$temp),
                         detail = detail)
        }
        updateProgress4 <- function(value = NULL,
                                    detail = NULL) {
            progress$inc(amount = 1 / nrow(default_comp_ingrid$temp),
                         detail = detail)
        }

        df_lvl2 <- data.frame(EAN = NA, ingredients_english = NA)
        #browser()
        num_of_cols <- dim(default_comp_ingrid$temp)[2]# - 1
        for (rows in 1:nrow(default_comp_ingrid$temp)) {
            df_lvl2[rows, 1] <-
                default_comp_ingrid$temp$EAN[rows] %>% as.character()

            l = 1
            for (cols in 2:num_of_cols) {
                
                lvl2_ingredients[l] <-
                    ifelse(
                        grepl(pattern = "\\([^,]+,.*\\)", default_comp_ingrid$temp[rows, cols]),
                        sapply(
                            default_comp_ingrid$temp[rows, cols],
                            func_remove_n_bracket,
                            level = input$parse_level - 1
                        ) %>%
                            paste0(collapse = ","),
                        ""
                    )

                
                l = l + 1
            }
            ##End of addition
            
            lvl2_ingredients <-
                paste(lvl2_ingredients,
                      sep = "",
                      collapse = ",")
            df_lvl2[rows, "ingredients_english"] <- lvl2_ingredients
            lvl2_ingredients <- NULL
        }
        
        
        
        ##end of function
        df_lvl2 %<>% na_if("") %>% na.omit()
        
        lvl2_ingredients_sep$data <-
            ingrid_sep(df_lvl2,
                       "EAN",
                       "ingredients_english",
                       updateProgress3)
        ingrid_counts_list_lvl2$data <-
            ingrid_count(lvl2_ingredients_sep$data$combined_list,
                         updateProgress4)

        new_parents_lvl2 <- reactiveValues(data = NULL)
        observeEvent(input$showAllIngredientsN, ignoreNULL = FALSE, {
            if (input$showAllIngredientsN) {
                new_parents_lvl2$data <- ingrid_counts_list_lvl2$data
            } else {
                new_parents_lvl2$data <-
                    ingrid_counts_list_lvl2$data[!ingrid_counts_list_lvl2$data$ingredient %in% tolower(parent_name_table_file()$ingredient), ]
            }
        })
        
        
        #View(new_parents_lvl2$data)
        
        ###
        output$table8 <- DT::renderDataTable(
            new_parents_lvl2$data,
            editable = T,
            server = T,
            options = list(
                searchHighlight = TRUE,
                columnDefs = list(
                    list(
                        targets = 3,
                        className = 'dt-center',
                        targets = 5,
                        render = JS(
                            "function(data, type, row, meta) {",
                            "return type === 'display' && data.length > 13 ?",
                            "'<span title=\"' + data + '\">' + data.substr(0, 13) + '...</span>' : data;",
                            "}"
                        )
                    )
                )
            ),
            caption = HTML(
                '<b>Table 1: Ingredient list extracted from the uploaded data</b>'
            ),
            style = "bootstrap",
            extensions = 'FixedHeader',
            colnames = c("Sl No", "Ingredients", "counts", "EAN")
        )
        output$select_rows_lvl2 <-
            renderPrint(new_parents_lvl2$data[input$table8_rows_selected, "ingredient"])
        
        ##table8 is editable
        proxy = dataTableProxy('table8')
        observeEvent(input$table8_cell_edit, {
            info = input$table8_cell_edit
            str(info)
            i = info$row
            j = info$col
            v = info$value
            lvl2_ingredients_sep$data$combined_list[lvl2_ingredients_sep$data$combined_list == as.character(new_parents_lvl2$data[i, j])] <-
                v
            
            ingrid_counts_list_lvl2$data[ingrid_counts_list_lvl2$data == as.character(new_parents_lvl2$data[i, j])] <-
                v
            
            ####
            new_parents_lvl2$data <-
                ingrid_counts_list_lvl2$data[!ingrid_counts_list_lvl2$data$ingredient %in% tolower(parent_name_table_file()$ingredient), ]
            ######
            
            #new_parents_lvl2$data[i,j] <<- DT::coerceValue(v, new_parents_lvl2$data[i,j])
            new_parents_lvl2$data %<>% group_by(ingredient) %>% mutate(counts = sum(counts)) %>% unique() %>% ungroup()
            
            replaceData(proxy, new_parents_lvl2$data, resetPaging = FALSE)
            # important
        })
        
        output$table9 <-
            DT::renderDataTable(
                parent_name_table_file(),
                options = list(
                    searchHighlight = TRUE,
                    ageLength = 5,
                    autoWidth = TRUE
                ),
                selection = list(mode = "single"),
                caption = HTML(
                    '<b>Table 2: Ingredient Thesaurus: Only the "Most common name" would be selected for assignment</b>'
                ),
                style = "bootstrap",
                colnames = c(
                    "Most common name",
                    "Other names",
                    "TimeAdded",
                    "PersonAdded"
                )
            )
        
        ###search-sort thesaurus added 2_18_2020
        
        sorted_thesaurus_lvl2 <- reactiveValues(data = NULL)
        observeEvent(input$table8_rows_selected, {
            #browser()
            #search = list()
            
            output$table9 <-
                DT::renderDataTable(
                    parent_name_table_file(),
                    selection = list(mode =
                                         "single"),
                    caption = HTML(
                        '<b>Table 2: Ingredient Thesaurus: Only the "Most common name" would be selected for assignment</b>'
                    ),
                    style = "bootstrap",
                    colnames = c(
                        "Most common name",
                        "Other names",
                        "TimeAdded",
                        "PersonAdded"
                    ),
                    options = list(
                        searchHighlight = TRUE,
                        search = list(
                            search = sub("(\\w)\\s.*", "\\1", new_parents_lvl2$data[c(input$table6_rows_selected)[1], "ingredient"])
                        )
                    )
                )
            
            ##New addition 12/17 for fuzzy match
            if (input$fuzzy_match_lvl2) {
                #print(input$fuzzy_match)
                #browser()
                search_term <-
                    new_parents_lvl2$data[c(input$table8_rows_selected)[1], "ingredient"]
                sorted_thesaurus2 <-
                    parent_name_table_file() %>% select(parent_name) %>% unique()
                similarity_score <-
                    data.frame(
                        sim_score = adist(
                            search_term,
                            sorted_thesaurus2$parent_name,
                            partial = F,
                            ignore.case = T
                        ) %>% t()
                    )
                sorted_thesaurus2 <-
                    bind_cols(sorted_thesaurus2, similarity_score)
                
                sorted_thesaurus_lvl2$data <- sorted_thesaurus2
                
                
                
                output$table9 <-
                    DT::renderDataTable(
                        sorted_thesaurus_lvl2$data,
                        selection = list(mode =
                                             "single"),
                        caption = HTML(
                            '<b>Table 2: Ingredient Thesaurus: Only the "Most common name" would be selected for assignment</b>'
                        ),
                        style = "bootstrap",
                        colnames = c("Most common name", "Similarity"),
                        options = list(
                            searchHighlight = TRUE,
                            order = list(list(2, 'asc'))
                        )
                    )
            }
            
        })
        
        ###Assign Parent_lvl2
        observeEvent(input$assign_parent_lvl2, {
            if (is.null(input$table9_rows_selected) |
                is.null(input$table8_rows_selected)) {
                createAlert(
                    session = session,
                    "no_parent_alert_lvl2",
                    title = "Select at least one entry from both tables",
                    style = "error",
                    append = F
                )
            } else {
                #browser()
                add_child <-
                    data.frame(
                        parent_name = sorted_thesaurus_lvl2$data[input$table9_rows_selected, "parent_name"],
                        ingredient = c(toupper(new_parents_lvl2$data[input$table8_rows_selected, "ingredient"])),
                        TimeAdded = as.character(Sys.time()),
                        PersonAdded = Sys.info()[8]
                    )
                
                #View(add_child)
                
                proxy = dataTableProxy('table8')
                new_parents_lvl2$data <-
                    new_parents_lvl2$data[-input$table8_rows_selected, ] %>% arrange(counts, ingredient)
                replaceData(proxy, new_parents_lvl2$data, resetPaging = FALSE)
                
                
                
                parent_name_table_file2 <-
                    reactive({
                        rbind(parent_name_table_file(),
                              as.data.frame(add_child)) %>% group_by() %>% arrange(parent_name, ingredient) %>% ungroup()
                    })
                
                write.csv(
                    parent_name_table_file2(),
                    thesaurusFilePath,
                    row.names = F
                )
            }
            
        })
        ###End of Addition 2_18_2020
        
        
        ###Add New Parent Level 2 2_18_2020
        observeEvent(input$add_new_parent_lvl2, {
            if (is.null(input$table8_rows_selected)) {
                createAlert(
                    session = session,
                    "new_parent_noname_alert_lvl2",
                    title = "Select at least one entry from table 1",
                    style = "error",
                    append = F
                )
            } else {
                ######
                ###Add text input here####
                #browser()
                new_parent_name_lvl2 <-
                    ifelse(
                        is.na(input$parent_name_lvl2) | input$parent_name_lvl2 == "" ,
                        toupper(new_parents_lvl2$data[c(input$table8_rows_selected), "ingredient"]),
                        toupper(input$parent_name_lvl2)
                    )
                #TimeAdded = Sys.time(),
                #PersonAdded = Sys.info()[8])
                #browser()
                ######
                add_parent <-
                    data.frame(
                        parent_name = new_parent_name_lvl2,
                        ingredient = toupper(new_parents_lvl2$data[c(input$table8_rows_selected), "ingredient"]),
                        TimeAdded = as.character(Sys.time()),
                        PersonAdded = Sys.info()[8]
                    )
                
                proxy = dataTableProxy('table8')
                new_parents_lvl2$data <-
                    new_parents_lvl2$data[-input$table8_rows_selected, ] %>% arrange(counts, ingredient)
                replaceData(proxy, new_parents_lvl2$data, resetPaging = FALSE)
                
                parent_name_table_file2 <-
                    reactive({
                        rbind(parent_name_table_file(), add_parent) %>% group_by() %>% arrange(parent_name, ingredient) %>% ungroup()
                    })
                write.csv(
                    parent_name_table_file2(),
                    thesaurusFilePath,
                    row.names = F
                )
                updateTextInput(
                    session = session,
                    inputId = "parent_name_lvl2",
                    label = "Add the most common name",
                    placeholder = "You can leave this blank",
                    value = NA
                )
            }
        })
        
        ### End of Adding New Parent 2_18_2020
        ###End of Addition 2_18_2020
        
        
    })
    
    ### End of Addition
    
}

# Run the application
#shinyApp(ui = ui, server = server)
