

# note --------------------------------------------------------------------

# It would use the google address to 

# code --------------------------------------------------------------------



library(data.table)
data <- readxl::read_excel("data.xlsx")
setDT(data)
data[, c("Lon", "Lat") := NA_real_]
i <- 1L
for (i in 1:nrow(data)) {
  if (!is.na(data[i, Adress])) {
    tmp <- ggmap::geocode(data[i, Adress], source = "google")
    set(data, i, "Lon", tmp$lon)
    set(data, i, "Lat", tmp$lat)
  }
}

readr::write_csv(data, "data.csv")
openxlsx::write.xlsx(data, "tmp.xlsx")
ggmap::geocode("165 Hydes Creek Rd，Bellingen NSW 2454, Australian", source = "google")

ggmap::geocode("", source = "google")


data <- readxl::read_excel("tmp.xlsx")
setDT(data)
i <- 1L
for (i in 1:nrow(data)) {
  if (!is.na(data[i, Adress]) & is.na(data[i, Lon])) {
    tmp <- ggmap::geocode(data[i, Adress], source = "google")
    set(data, i, "Lon", tmp$lon)
    set(data, i, "Lat", tmp$lat)
  }
}
