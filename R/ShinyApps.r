# Graphical user interface to application modules
# nocov start

ModuleAApp = function() {

  appDir = system.file("ModuleAApp", package = "MedianaADT")
  shiny::runApp(appDir, display.mode = "normal")

}

ModuleBApp = function() {

  appDir = system.file("ModuleBApp", package = "MedianaADT")
  shiny::runApp(appDir, display.mode = "normal")

}

ModuleCApp = function() {

  appDir = system.file("ModuleCApp", package = "MedianaADT")
  shiny::runApp(appDir, display.mode = "normal")

}

ModuleDApp = function() {

  appDir = system.file("ModuleDApp", package = "MedianaADT")
  shiny::runApp(appDir, display.mode = "normal")

}

ModuleEApp = function() {

  appDir = system.file("ModuleEApp", package = "MedianaADT")
  shiny::runApp(appDir, display.mode = "normal")

}

# nocov end
