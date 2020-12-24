# functions passed to targets
get_lsay2003_data <- function(){
  lsay2003 = cloud_get(user = cloudstor_user,
                       password = cloudstor_password,
                       dest = 'lsay2003.sav',
                       cloud_address = 'https://cloudstor.aarnet.edu.au/plus/remote.php/webdav/Databases/LSAY-v10/LSAY2003/lsay2003.sav')
  
  return(lsay2003)
}



# Global functions ####
library(purrr)

# Create versions of functions with na.rm=TRUE as default
Funs <- Filter(is.function,sapply(ls(baseenv()),get,baseenv()))
na.rm.f <- names(Filter(function(x) any(names(formals(args(x)))%in% 'na.rm'),Funs))
na.rm.f <- c(na.rm.f, "mean", "sd")

# Create strings. Dot "." is optional
fs <- lapply(na.rm.f,
             function(x) paste0(".", x, "=purrr::partial(", x ,", na.rm = T)"))

eval(parse(text = fs)) 

.scale <- function(x){(x - .mean(x))/.sd(x)}

cloud_get <- function(user, password, cloud_address, dest){
  p = file.path(tempdir(), dest)
  h = curl::new_handle()
  curl::handle_setopt(h, username = user)
  curl::handle_setopt(h, password = password) 
  
  curl::curl_download(cloud_address, p, handle = h)
  
  d = readit::readit(p)
  
  return(d)
  
}
