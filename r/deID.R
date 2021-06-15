# Location Analysis and De-identification
# Written by Trisha R. Shrum

d <- r

#### IP Address Analysis ####

# Identifying duplicate IP Addresses
d$duplicatedIPAddress <- duplicated(d$IPAddress)

# Pulling zip code and state information from IP Address based location data
l <- data.frame(ResponseId = d$ResponseId, reverse_geo(lat = d$LocationLatitude, long = d$LocationLongitude))

l$zip_IPAddress <- str_extract(l$address, '[0-9]{5}')

Pattern <- paste0(paste0(".*\\b(", paste0(state.name, collapse="|")), ")\\b.*")
l$state_IPAddress <- sub(Pattern, "\\1", l$address)

d %>%
  left_join(l, by = "ResponseId") -> d

# Removing participants with IP addresses outside the US
unique(d$state_IPAddress)
d %>%
  filter(state_IPAddress == "Ooty-Coonoor Road, Udhagamandalam, The Nilgiris District, Tamil Nadu, 643001, India" |
           state_IPAddress == "Christ the King, Dr Nanjappa Road, AIT Colony, Ward 72, Central Zone, Coimbatore, Coimbatore North, Coimbatore District, Tamil Nadu, 641001, India" |
           state_IPAddress == "Ekambareswara Agraharam Street, Sowcarpet, Georgetown, Zone 5 Royapuram, Chennai, Chennai District, Tamil Nadu, 600079, India") -> drop  

d %>%
  filter(state_IPAddress != "Ooty-Coonoor Road, Udhagamandalam, The Nilgiris District, Tamil Nadu, 643001, India" &
           state_IPAddress != "Christ the King, Dr Nanjappa Road, AIT Colony, Ward 72, Central Zone, Coimbatore, Coimbatore North, Coimbatore District, Tamil Nadu, 641001, India" &
           state_IPAddress != "Ekambareswara Agraharam Street, Sowcarpet, Georgetown, Zone 5 Royapuram, Chennai, Chennai District, Tamil Nadu, 600079, India" |
           is.na(state_IPAddress)) -> d  


#### De-identifying data ####
d %>%
  dplyr::select(-IPAddress, -state_IPAddress, -zip_IPAddress, -LocationLatitude, -LocationLongitude, -lat, -long, -address) -> d

#### Saving deidentified data and removing identifying data files
write_csv(d, "data/UVM_SEGS_COVID19_Part1_rawData_noID.csv")
rm(r, d, l)

# Cleanup R environment
rm(Pattern)
