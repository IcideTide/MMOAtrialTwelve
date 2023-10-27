# trialTwelve

# current task:
# - revert the resistance to herbicide drop down menus code to pre-partner filtering code
  # - ie. when it was faster but included partners that did not meet the criteria which were filtered out before they were out put to the table
# * Haven't made any significant changes yet


library(shiny)
library(bslib)
library(DT)
library(readxl)
library(tidyverse)

# changed this code to draw from the local extractProduct.csv file to speed up the startup
#url <- "https://pest-control.canada.ca/pesticide-registry-api/api/extract/product"
# download the excel file and read it into R
#tempFile <- tempfile()    # create a temporary file
#download.file(url, tempFile)    # download the excel file
#primeExtractProduct <- read_csv(tempFile)   # read the excel file into r
#file.remove(tempFile)

primeExtractProduct <- read_csv("product_extract_copy.csv")

startUpProcesses <- function(primeExtractProduct) {
  crop_key_in <- read_csv("crop_key_in_copy.csv") %>%
    select(-`row_number()`)
  mainTankMixPartner <- read_csv("recursiveTMP_copy.csv") %>%
    select(-`row_number()`)
  pests_key_in <- read_csv("pests_key_in_copy.csv") %>%
    select(-`row_number()`)
  group_key_in <- read_csv("group_key_in_copy.csv") %>%
    select(-`row_number()`)
  
  remove_trademark <- function(x) {
    gsub("\xae", "", x)
  }
  
  subPrimeExtractProduct <- primeExtractProduct %>%
    filter(`Current / Historical` == "Current" & 
             `Registration Status` %in% c("Full Registration", "Emergency Registration") &
             `Product Type` == "HERBICIDE" &
             !`Marketing type` %in% c("TECHNICAL ACTIVE", "MANUFACTURING CONCENTRATE", "DOMESTIC")) %>%
    mutate(`Product name - English` = remove_trademark(`Product name - English`),
           `Product name - French` = remove_trademark(`Product name - French`)) %>%
    select(-`Registration Status` &
             -`Expiry date` &
             -`Marketing type` & 
             -`Date first registered` &
             -`Exclusive period start date` & 
             -`Product Type` &
             -`Registrant name` &
             -`Use Site Category` & 
             -`Current / Historical`)
  
  collapsedTankMixPartners <- mainTankMixPartner 
  # drop_na(Reg_Num_f)
  
  modExtractProduct <- subPrimeExtractProduct %>%
    separate_longer_delim(`Sites of Use`, delim = ";")  %>% # this also separates the Sites of Use based on the semi-colon
    separate_longer_delim(`Sites of Use`, delim = ",") %>% # it splits the `Sites of Use` around semi-colons and commas
    left_join(crop_key_in,
              join_by(`Sites of Use`),
              copy = FALSE,
              suffix = c(".x", ".y"), 
              keep = FALSE,
              na_matches = "never",
              unmatched = "drop",
              relationship = "many-to-many") %>%
    drop_na(Crop_Name) %>%
    select(-`Sites of Use`) %>%
    distinct() %>%
    group_by(`Registration number`, 
             `Product name - English`,
             `Product name - French`,
             `Active ingredients - English`,
             `Active ingredients - French`,
             `Pests`) %>%
    summarize(`Sites of Use` = paste0(Crop_Name, collapse = ";")) %>%
    left_join(collapsedTankMixPartners,
              by = join_by(`Registration number` == `Registration number`),
              copy = FALSE,
              keep = FALSE) %>%
    separate_longer_delim(`Pests`, delim = ";") %>%
    left_join(pests_key_in,
              by = join_by(Pests == Pests),
              copy = FALSE,
              suffix = c(".x", ".y"), 
              keep = FALSE,
              na_matches = "never",
              unmatched = "drop",
              relationship = "many-to-many") %>%
    select(-Pests & -Pests_Key) %>%
    group_by(`Registration number`, 
             `Product name - English`,
             `Product name - French`,
             `Active ingredients - English`,
             `Active ingredients - French`,
             `Sites of Use`,
             Reg_Num_f,
             Notes) %>%
    summarize(Pests_Sub_Key = paste0(Pests_Sub_Key, collapse = ";"),
              Sci_Name = paste0(Sci_Name, collapse = ";"),
              Res_MOA_Group = paste0(Res_MOA_Group, collapse = ";")) %>%
    separate_longer_delim(`Active ingredients - English`, delim = ";") %>%
    left_join(group_key_in,
              by = join_by(`Active ingredients - English` == `Active ingredients - English`),
              copy = FALSE,
              suffix = c(".x", ".y"), 
              keep = FALSE,
              na_matches = "never",
              unmatched = "drop")  %>%
    ungroup() %>%
    select(-`Active ingredients - English`) %>%
    group_by(`Registration number`, 
             `Product name - English`,
             `Product name - French`,
             `Active ingredients - French`,
             `Sites of Use`,
             Reg_Num_f,
             Notes,
             Pests_Sub_Key,
             Sci_Name,
             Res_MOA_Group) %>%
    summarize(`Active ingredients - English_Key` = paste0(`Active ingredients - English_Key`, collapse = " & "),
              Group_Number = paste0(Group_Number, collapse = " & "),
              MOA = paste0(MOA, collapse = " & ")) %>%
    relocate(`Active ingredients - English_Key`, .before = `Active ingredients - French`) %>%
    relocate(Group_Number, .before = `Active ingredients - English_Key`) %>%
    relocate(MOA, .before = `Active ingredients - English_Key`)
}

masterExtractProduct <- startUpProcesses(primeExtractProduct)

ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # Application title
  titlePanel("Trial Eleven"),
  
  # 
  sidebarLayout(
    sidebarPanel(
      selectInput("cropSelect", "Select a crop", choices = c("Select a crop" = "", 
                                                             as.list(unique(unlist(str_split(masterExtractProduct$`Sites of Use`, pattern = "\\;"))))), 
                  multiple = FALSE),
      
      # let's try some selectizeInput drop down menus
      # selectInput("pestSelect", "Select a weed", choices = c("Select a weed" = "", NULL), 
      #             multiple = FALSE),
      selectizeInput("pestSelect", 
                     label = "Select a weed", 
                     choices = c("Select a weed" = "", NULL),
                     options = list(
                       placeholder = "Select a weed",
                       multiple = FALSE)),
      
      selectizeInput("resSelect",
                     label = "Select a known resistance",
                     choices = c("Select a known resistance" = "", NULL),
                     options = list(
                       placeholder = "Select a known resistance",
                       multiple = FALSE)),
      
      selectizeInput("herbSelect",
                     label = "Select a herbicide",
                     choices = c("Select a herbicide" = "", NULL),
                     options = list(
                       placeholder = "Select a herbicide",
                       multiple = FALSE))
    ),
    
    mainPanel(
      DT::dataTableOutput("data")
    )
  )
)


server <- function(input, output) {
  
  # create a data frame with the user selected crop filtered for
  crop <- reactive({
    
    req(input$cropSelect)
    
    masterExtractProduct[grep(input$cropSelect, masterExtractProduct$`Sites of Use`), ] %>%
      separate_longer_delim(`Sites of Use`, delim = ";") %>%
      separate_longer_delim(`Sites of Use`, delim = ",") %>%
      filter(`Sites of Use` == input$cropSelect) %>%
      distinct()
  })
  
  # once the data frame crop() is filled create a data frame with the pest options that updates the pestSelect drop down menu
  observeEvent(crop(), {
    
    print("Crop changed")
    
    isolate({
      choicesPests <- crop() %>%
        separate_longer_delim(c(Pests_Sub_Key, Sci_Name, Res_MOA_Group), delim = ";") %>%
        ungroup() %>%
        select(Pests_Sub_Key) %>%
        distinct() %>%
        arrange(Pests_Sub_Key)
      
      # changed this in kind with the changed selectizeInput in the UI
      updateSelectizeInput(inputId = "pestSelect", 
                           choices = c("Select a weed" = "", choicesPests),
                           options = list(
                             placeholder = "Select a weed",
                             allowEmptyOption = TRUE))
      
      updateSelectizeInput(inputId = "resSelect",
                           choices = c("Select a known resistance" = "", NA),
                           options = list(
                             placeholder = "Select a known resistance",
                             allowEmptyOption = TRUE))
      
      updateSelectizeInput(inputId = "herbSelect",
                           choices = c("Select a herbicide" = ""),
                           options = list(
                             placeholder = "Select a herbicide",
                             allowEmptyOption = TRUE))
    })
  })
  
  
  # create a data frame with both user selected crop and pest filtered for
  pest <- reactive({
    req(input$pestSelect)
    crop() %>%
      separate_longer_delim(c(Pests_Sub_Key, Sci_Name, Res_MOA_Group), delim = ";") %>%
      filter(Pests_Sub_Key == "REFER TO LABEL" | Pests_Sub_Key == input$pestSelect) %>%
      distinct() %>%
      group_by(`Registration number`, 
               `Product name - English`,
               `Product name - French`,
               Group_Number,
               MOA,
               `Active ingredients - English_Key`,
               `Active ingredients - French`,
               `Sites of Use`,
               Reg_Num_f) %>%
      summarize(Pests_Sub_Key = paste0(Pests_Sub_Key, collapse = ";"),
                Sci_Name = paste0(Sci_Name, collapse = ";"),
                Res_MOA_Group = paste0(Res_MOA_Group, collapse = ";"))
    
  })
  
  # once the data frame pest() is filled create a data frame with the known herbicide resistances that updates the herbselect drop down menu
  observeEvent(pest(), {
    
    print("Pest changed")
    
    isolate({
      choicesResistances <- pest() %>%
        separate_longer_delim(Res_MOA_Group, delim = ";") %>%
        separate_longer_delim(Res_MOA_Group, delim = ",") %>%
        ungroup() %>%
        select(Res_MOA_Group) %>%
        distinct() %>%
        arrange(Res_MOA_Group)
      
      # freezeReactiveValue(input, "resSelect")
      
      updateSelectizeInput(inputId = "resSelect",
                           choices = c("Select a known resistance" = "", NA, choicesResistances),
                           options = list(
                             placeholder = "Select a known resistance",
                             allowEmptyOption = TRUE))
    })
  })
  
  
  # create a data frame with user selected crop, pest, and group resistance selected
  # this is used to populate the herbicide drop down menu
  resist <- reactive({
    req(input$resSelect)
    
    # 1. create a variable with the selected resistances
    selectedRes <- input$resSelect %>%
      str_split("\\+") %>%
      unlist() %>%
      as.character()
    
    # 2. Create a data frame with the primary herbicide filtered of the selected resistance group(s)
    resist1 <- pest() %>%
      filter(!str_detect(Group_Number, paste(selectedRes, collapse = "|")))
    
    # 3. create a value with the resist1 data frame and retain only primary herbicide registration numbers
    resist1RegNum <- resist1 %>%
      ungroup() %>%
      select(`Registration number`) %>%
      unlist() %>%                             # its important that it is not a data frame, unlist() makes it a value
      as.character()
    
    # 4. use masterExtractProduct and remove the herbicides that appear in resist1RegNum and retain only the Registration numbers and retain as a value
    antiSelection <- masterExtractProduct %>%
      ungroup() %>%                           
      select(`Registration number`,
             Group_Number) %>%
      filter(str_detect(Group_Number, paste(selectedRes, collapse = "|"))) %>%
      select(`Registration number`) %>%
      filter(!str_detect(`Registration number`, paste(resist1RegNum, collapse = "|"))) %>%
      unlist() %>%
      as.character()
    
    # 5. use the antiSelection value and the selectedRes to remove tank mix partners that don't match the user selected criteria
    resistSplit <- resist1 %>%
      ungroup() %>%
      select(`Registration number`,
             `Product name - English`,
             Group_Number,
             MOA,
             Pests_Sub_Key,
             Reg_Num_f) %>%
      separate_longer_delim(Reg_Num_f, delim = ">") %>%
      drop_na(Reg_Num_f) %>%
      #filter(!str_detect(Group_Num_f, paste(selectedRes, collapse = "|"))) %>% # filter out the selected group resistance from the tank partner this time
      filter(!str_detect(Reg_Num_f, paste(antiSelection, collapse = "|"))) # filter out any registration number that appears in the variable diffAntiSelection
    # drop_na(Reg_Num_f)
  })
  
  
  # once the data frame pest() is filled create a data frame with the herbicide options that updates the herbSelect drop down menu
  observeEvent(resist(), {
    
    print("Resistance changed")
    
    # isolate({
    # choicesHerb <- as_tibble(resist()$`Product name - English`) %>%
    #   distinct()
    
    choicesHerb <- resist() %>%
      drop_na(Reg_Num_f) %>%
      select(`Product name - English`) %>%
      distinct()
    
    # updateSelectInput(inputId = "herbSelect", choices = c("Select a herbicide" = "", choicesHerb))
    
    updateSelectizeInput(inputId = "herbSelect",
                         choices = c("Select a herbicide" = "", choicesHerb),
                         options = list(
                           placeholder = "Select a herbicide",
                           allowEmptyOption = TRUE))
    
    
    
    freezeReactiveValue(input, "herbSelect")
    
    # })
  })
  
  
  # so at this point we have the herbicide selected based on the user selected crop, pest, and known group resistance
  # the next major step is to grab the info for that herbicide from the masterExtractProduct
  # then separate out the tank mix partners like you're currently doing in the renderTable below
  # you need to do it up here so that you can get join the partners up with their group number and eliminate the ones that have the same group number as the selected resistance
  
  
  # takes the resist() dataframe and filters the user selected herbicide
  herb <- reactive({
    req(input$herbSelect)
    
    # there needs to be an if statement here in case there are no options for a specific selection
    if (nrow(resist()) == 0) {
      validate("There are no tank mix options for this combo")
    } else {
      mixOptions <- resist() %>%
        filter(`Product name - English` == input$herbSelect) %>%
        ungroup() %>%
        separate_longer_delim(c(Reg_Num_f), delim = ";") %>%
        mutate(Reg_Num_f2 = as.double(str_extract_all(Reg_Num_f, "[0-9]+\\.*[0-9]*")),
               Reg_Sym = str_extract(Reg_Num_f, "\\+"),
               Action_Word = Reg_Sym,
               Action_Word = str_replace(Action_Word, "\\+", "and"),
               Action_Word = replace_na(Action_Word, "Add")) %>%
        select(`Registration number`,
               `Product name - English`,
               Group_Number,
               MOA,
               Pests_Sub_Key,
               Action_Word,
               Reg_Num_f2) %>%
        left_join(masterExtractProduct,                              # matches tank mix partner registration numbers with the master data frame
                  by = join_by(Reg_Num_f2 == `Registration number`),
                  copy = FALSE,
                  keep = FALSE) %>%
        ungroup() %>%
        select(`Registration number`,
               `Product name - English.x`,
               Group_Number.x,
               MOA.x,
               Pests_Sub_Key.x,
               Action_Word,
               Reg_Num_f2,
               `Product name - English.y`,
               Group_Number.y,
               MOA.y,
               Pests_Sub_Key.y) %>%
        mutate(Pests_Sub_Key1.y = str_extract(Pests_Sub_Key.y, input$pestSelect),
               Pests_Sub_Key2.y = str_extract(Pests_Sub_Key.y, "REFER TO LABEL")) %>%
        unite(Pests_Sub_Key.y, Pests_Sub_Key1.y, Pests_Sub_Key2.y, sep = " ") %>%
        mutate(Pests_Sub_Key.y = str_replace(Pests_Sub_Key.y, "NA", "")) %>%
        drop_na(Reg_Num_f2)
      
      if (nrow(mixOptions) == 0) {
        validate("There are no tank mix options for this combo")
      } else {
        mixOptions 
      }
      
    }
    
  })
  
  
  
  output$data <- DT::renderDataTable({
    herb()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
