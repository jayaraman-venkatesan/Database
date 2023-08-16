library("XML")

result <- tryCatch({
  
      xml_data <- xmlParse("VenkatesanJ.CS5200.SuF.Grants.xml")
      
      # Finding grants for researcher id = 001
      
      grants_count <- xpathApply(xml_data, "//grants/Content[@rid='001']",xmlGetAttr,"rid")
      
      print(length(grants_count))
  
      },  
     error = function(e) {
      # Assert that the error message matches our expectation
      print("Cannot process the XML")
    })
