
SetupGEE<-function(){
  # Get the admin boundaries - GAUL - using Google Earth Engine
  library(reticulate)
  # use_python(Sys.which("python3"))
  if (!("rgee" %in% installed.packages()[,1])) {
    print("Installing package `rgee`...")
    install.packages("rgee")
    
    library(rgee) 
    ee_install(py_env = "/home/hamishwp/anaconda3/bin/python3")
    Sys.setenv("EARTHENGINE_GCLOUD" = gee_bin)
    py_install( "earthengine-api==0.1.277", "rgee")
    ee_check()
    rgee::ee_install_set_pyenv(py_path = "/usr/bin/python3", py_env="rgee")
    # reticulate::import("sklearn")
    
  } else { 
    library(rgee) 
  }
  
  ee$Authenticate(auth_mode='notebook')
  ee$Initialize(project='ee-hamishpatten')
  # ee_Initialize(user = gee_user, project='ee-hamishpatten', drive = TRUE)
  
  return(T)
  
}
# Setup Google Earth Engine library and objects
SetupGEE()


# library(reticulate)
# # Check which version of python3 you're using with
# python3 --version
# # Let's say we use python3.11, then we run:
# sudo apt-get install python3.11-venv
# use_python("/usr/bin/python3")
# # Then run
# py_install( "earthengine-api==0.1.277", "rgee")
# use_python("/home/hamishwp/anaconda3/bin/python3")
# rgee::ee_install_set_pyenv(py_path = "/home/hamishwp/anaconda3/bin/python3", py_env="rgee")

# Sys.which("python3")
# library(reticulate)
# reticulate::use_python("/home/hamishwp/.local/share/r-miniconda/envs/r-reticulate/bin/python")