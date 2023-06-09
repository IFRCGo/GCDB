
SetupGEE<-function(){
  # Get the admin boundaries - GAUL - using Google Earth Engine
  library(reticulate)
  # use_python(Sys.which("python3"))
  if (!("rgee" %in% installed.packages()[,1])) {
    print("Installing package `rgee`...")
    install.packages("rgee")
    
    library(rgee) 
    ee_install()
    Sys.setenv("EARTHENGINE_GCLOUD" = gee_bin)
    py_install( "earthengine-api==0.1.277", "rgee")
    ee_check()
    rgee::ee_install_set_pyenv(py_path = "/usr/bin/python3", py_env="rgee")
    # reticulate::import("sklearn")
    
  } else { 
    library(rgee) 
  }
  
  ee_Initialize(user = gee_user, drive = TRUE)
  
  return(T)
  
}
