#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("libraries.R")
source("readFiles.R")

library(shiny)

# Define UI for application that draws a histogram

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            text = "Input File",
            tabName = "tab1"
        ),
        menuItem(
            text = "File Preview/Parse",
            tabName = "tab2"
        ),
        menuItem(
            text = "Missing Brackets",
            tabName = "tab3"
        ),
        menuItem(
            text =  "Build Thesaurus",
            tabName = "tab7"
        ),
        menuItem(
            text =  "Level n Parsing",
            tabName = "tab8"
        ),
        menuItem(
            text =  "Download Data",
            tabName = "tab6"
        )
        # ),
        # menuItem(
        #   text =  "Do_stuffs2",
        #   tabName = "tab8"
        # )
    )
)


body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "tab1",
            fileInput(inputId = "ingrid_file", label = "Step:1 Upload Ingredient File")
            #DTOutput(outputId = "file_read")
            
        ),
        tabItem(
            tabName = "tab2",
            actionButton(inputId = "process0", label = "Step:2 Parse Ingredients", icon = icon("tick")),
            DTOutput(outputId = "table1")
        ),
        
        tabItem(
            tabName = "tab3",
            fluidPage(
                fluidRow(
                    column(
                        width = 3,
                        actionButton(inputId = "process1", label = "Step:3 Update", icon = icon("tick"))
                    ),
                    column(
                        width = 6,
                        HTML("<font color='red'><b>Once you have fixed the brackets it is recommended that you go to the <i>Download Data</i> tab and download a temporary copy of the Ingredient List file so that you'd have a back-up in case the program crashes while assigning the parent names.</b></font>")
                    )
                )
            ),
            
            
            # box(
            #   title = "Download modified file with corrected brackets",
            #   textInput(inputId = "file_name_brackets", label = "Enter the name of the corrected raw file"),
            #   downloadButton(outputId = "downloadBrackets_corrected", "Download file with corrected brackets")
            # ),
            DTOutput(outputId = "table2", height = "95%", width = "75%")
            
        ),
        
        tabItem(
            tabName = "tab7",
            fluidPage(
                fluidRow(
                    column(
                        width = 2,
                        actionButton(inputId = "count_compute2", label = "Step:4 Compute Count", icon = icon("plus"))
                    ), column(
                        width = 2,
                        actionButton(inputId = "modify", label = "Modify Description", icon = icon("question"))
                    ), column(
                        width = 3,
                        box(
                            width = "100%",
                            textInput(inputId = "text_omit", label = "Add a word to omit", placeholder = "Enter word to omit here", width = "100%"),
                            actionButton(inputId = "omit_btn", label = "Omit", icon = icon("substract"))
                        )
                    ), column(
                        width = 5,
                        #valueBoxOutput(outputId = "selected_ingredients")
                        #textOutput(outputId = "select_rows"),
                        verbatimTextOutput(outputId = "select_rows")
                    )
                ),
                HTML('<hr>'),
                
                bsAlert("no_parent_alert"),
                bsAlert("new_parent_noname_alert"),
                bsAlert("food_select_modify_alert"),
                bsModal(id = "Edit_food", "Edit Description", trigger = "modify", size = "large",
                        textAreaInput(inputId = "edit_food2", label = "Edit description of the food", width = 800, height = 500, placeholder = "Select a single row in table 1 to modify"),  #DTOutput(outputId = "mod_food",height = "95%"),
                        actionButton(inputId = "modify_food_confirm", label = "Parse", icon = icon("multiply")),
                        bsAlert("mod_failed")),
                
                fluidRow(column(width = 5, DTOutput(outputId = "table6",height = "95%")),
                         column(width = 5, DTOutput(outputId = "table7",height = "95%")),
                         column(width = 2, 
                                br(), br(), br(), br(),
                                (actionButton(inputId = "assign_parent", label = "Assign Parent")),
                                br(), br(), br(), br(),
                                box(
                                    width = "100%",
                                    textInput(inputId = "parent_name", label = "Add the most common name", placeholder = "You can leave this blank", width = "100%"),
                                    actionButton(inputId = "add_new_parent", label = "Add New Parent")),
                                br(), br(), br(),
                                switchInput(inputId = "showAllIngredients1", label = "Show All Ingredients", value = F),
                                br(),
                                checkboxInput(inputId = "fuzzy_match", label = "Do Fuzzy match", value = T)
                                #switchInput(inputId = "fuzzy_match", label = "Do Fuzzy match")
                         )
                         
                ),
                hr()
            )
        ),
        tabItem(
            tabName = "tab8",
            fluidPage(
                fluidRow(
                    column(
                        width = 3,
                        fileInput(inputId = "lvl1_parsed", label = "Upload Ingredient Parsed File from level 1")
                    ),
                    column(
                        width = 3,
                        actionButton(inputId = "count_compute_lvl2", label = "Step:5 Compute Count for level n", icon = icon("plus"))
                    ),
                    column(width = 3,
                           sliderInput(inputId = "parse_level", label = "Select Parsing Level (Default = 2)", min = 2, max = 5, step = 1, value= 2))
                ),
                column(width = 3,
                       verbatimTextOutput(outputId = "select_rows_lvl2")),
                br(), br(), br(), br(),
                bsAlert("no_parent_alert_lvl2"),
                bsAlert("new_parent_noname_alert_lvl2"),
                fluidRow(column(width = 5, DTOutput(outputId = "table8",height = "95%")),
                         column(width = 5, DTOutput(outputId = "table9",height = "95%")),
                         column(width = 2, 
                                br(), br(), br(), br(),
                                (actionButton(inputId = "assign_parent_lvl2", label = "Assign Parent")),
                                br(), br(), br(), br(),
                                box(
                                    width = "100%",
                                    textInput(inputId = "parent_name_lvl2", label = "Add the most common name", placeholder = "You can leave this blank", width = "100%"),
                                    actionButton(inputId = "add_new_parent_lvl2", label = "Add New Parent")),
                                br(), br(), br(),
                                switchInput(inputId = "showAllIngredientsN", label = "Show All Ingredients", value = F),
                                br(),
                                checkboxInput(inputId = "fuzzy_match_lvl2", label = "Do Fuzzy match", value = T)
                         )
                )
            )
        ),
        tabItem(
            tabName = "tab6",
            fluidPage(
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = "Download Files for level 1",
                            textInput(inputId = "file_name_ingrid", label = "Enter the name of the ingredient file"),
                            downloadButton(outputId = "downloadData1", "Step:6 Download Ingredient list"),
                            textInput(inputId = "file_name_ingrid_count", label = "Enter the name of the ingredient count file"),
                            downloadButton(outputId = "downloadData2", "Step:7 Download Ingredient counts")
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            title = "Download Files for Level n",
                            textInput(inputId = "file_name_ingrid_lvl2", label = "Enter the name of the ingredient file for level n"),
                            downloadButton(outputId = "downloadData1_lvl2", "Step:8 Download Ingredient list for level n"),
                            textInput(inputId = "file_name_ingrid_count_lvl2", label = "Enter the name of the ingredient count file for level n"),
                            downloadButton(outputId = "downloadData2_lvl2", "Step:9 Download Ingredient counts for level n")
                        )
                    )
                )
            )
        )
    )
)


###
ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body)


