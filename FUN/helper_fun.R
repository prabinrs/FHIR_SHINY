library(httr)
library(jsonlite)
library(fhircrackr)

fetchPatients <- function() {
  response<- fhir_url(url = base_url, resource = "Patient")
  patient_bundles<- fhir_search(request = response, max_bundles = 2, verbose = 0)
}

