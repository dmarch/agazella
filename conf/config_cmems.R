#---------------------------------------------------------------------
# config_cmems.R          Configuration for downloading CMEMS
#---------------------------------------------------------------------



# set user keys to use the download services
# read user and password from json file (not in the Github repo)
# alternative is to set up manually:
# keys <- data.frame(user = "YOURUSER", pwd = "YOURPASSWORD")
keyfile = "auth/cmems_keys.json"
keys <- jsonlite::fromJSON(keyfile)


# set path to the MOTU script
motu_path <- "C:/motuclient-python/motuclient.py"
