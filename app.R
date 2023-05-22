library(shiny)
library(httr)
library(jsonlite)
library(fhircrackr)

# Define the base URL of the HAPI FHIR server (R4 version)
base_url <- "http://hapi.fhir.org/baseR4/"
headers = c(
  `accept` = "application/fhir+json"
)

# Function to fetch patient resources from the FHIR server
fetchPatients <- function() {
  response<- fhir_url(url = "http://hapi.fhir.org/baseR4/", resource = "Patient")
  patient_bundles<- fhir_search(request = response, max_bundles = 2, verbose = 0)
  
  # Design tables 
  table_description <- fhir_table_description(
    resource = "Patient",
    cols     = c(
      PID         = "id",
      use_name    = "name/use",
      given_name  = "name/given",
      family_name = "name/family",
      gender      = "gender",
      birthday    = "birthDate"
    ),
    sep           = " ~ ",
    #brackets      = c("<<<", ">>>"),
    rm_empty_cols = FALSE,
    format        = 'compact',
    keep_attr     = FALSE
  )
  
  patients<- fhir_crack(bundles = patient_bundles, 
                        design = table_description, 
                        verbose = 0)
  return(patients)
}

# Function to save data to the FHIR server
saveData <- function(data) {
  url <- paste0(base_url, "Observation")
  response <- POST(url, body = data, add_headers("Content-Type" = "application/json"))
  if (http_type(response) == "application/json") {
    message("Data saved successfully!")
  } else {
    stop("Failed to save data.")
  }
}

# UI definition
ui <- fluidPage(
  titlePanel("FHIR Patient Form"),
  sidebarLayout(
    sidebarPanel(
      selectInput("patient", "Select a Patient:", choices = NULL),
      textInput("name", "Patient Name:"),
      dateInput("birthdate", "Birthdate:"),
      actionButton("saveBtn", "Save")
    ),
    mainPanel(
      h3("Selected Patient Details"),
      verbatimTextOutput("patientInfo")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Fetch patient resources from the FHIR server
  patients <- reactive({
    fetchPatients()
  })
  
  # Populate the patient dropdown with fetched patients
  observe({
    patientNames <- patients$given_name
    updateSelectInput(session, "patient", choices = patientNames)
  })
  
  # Display the selected patient details
#   observe({
#     if (!is.null(input$patient)) {
#       patientData <- patients
#       patientIndex <- match(input$patient, patients$given_name)
#       patient <- patientData[patientIndex]
#       output$patientInfo <- renderPrint({
#         patient
#       })
#     }
#   })
#   
#   # Save the form data
#   observeEvent(input$saveBtn, {
#     if (!is.null(input$patient)) {
#       patientData <- fromJSON(patients())
#       patientIndex <- match(input$patient, sapply(patientData$entry, function(entry) entry$resource$name[[1]]$given))
#       patientId <- patientData$entry[[patientIndex]]$resource$id
#       observation <- list(
#         resourceType = "Observation",
#         subject = list(reference = paste0("Patient/", patientId)),
#         status = "final",
#         code = list(coding = list(
#           system = "http://loinc.org",
#           code = "9279-1",
#           display = "Pain severity"
#         )),
#         valueQuantity = list(
#           value = input$name,
#           unit = "",
#           system = "http://unitsofmeasure.org",
#           code = "{score}"
#         ),
#         effectiveDateTime = as.character(Sys.time())
#       )
#       saveData(toJSON(observation))
#     }
#   })
}

# Run the application
shinyApp(ui = ui, server = server)
