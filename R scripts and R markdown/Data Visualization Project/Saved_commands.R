setwd("G:/My Drive/Summer 22/Project")
police_reports = read.csv("Incident_Reports__2018_to_Jul_22.csv")
library(tidyverse)
library(lubridate)

Incident_Dates = as.POSIXct(police_reports$Incident.Datetime,
                            format="%m/%d/%Y  %H:%M")
Incident.Month = month(Incident_Dates)
police_reports$Incident.Month = Incident.Month

vehicle_incidents = subset(police_reports, 
                      Incident.Subcategory == "Larceny - From Vehicle")
vehicle_incidents$Incident.Date = as.Date(vehicle_incidents$Incident.Date, 
                                          format="%m/%d/%Y")

count_by_month = count(vehicle_incidents, Incident.Month)[-13,]
count_by_weekday = count(vehicle_incidents, Incident.Day.of.Week)
count_by_date = count(vehicle_incidents, Incident.Date)
count_by_time = count(vehicle_incidents, Police.District)



v_incident_18 = subset(vehicle_incidents, Incident.Year == 2018)
v_incident_19 = subset(vehicle_incidents, Incident.Year == 2019)
v_incident_20 = subset(vehicle_incidents, Incident.Year == 2020)
v_incident_21 = subset(vehicle_incidents, Incident.Year == 2021)

v_18_time_count = count(v_incident_18, Incident.Time)
v_19_time_count = count(v_incident_19, Incident.Time)
v_20_time_count = count(v_incident_20, Incident.Time)
v_21_time_count = count(v_incident_21, Incident.Time)

ggplot(data = v_18_date_count, aes(Incident.Date, n)) +
  geom_point() +
  geom_point(mapping = aes(v_19_date_count$Incident.Date,v_19_date_count$n,
                           color = "Green")) +
  geom_point(mapping = aes(v_21_date_count$Incident.Date,v_21_date_count$n,
                           color = "Blue"))
ggplot(data = count_by_time, aes(Incident.Time,n)) +
  geom_point()

source("constrict_time.R")
constricted_time_count = constrict_time(count_by_time$Incident.Time)
constricted_time_count
ggplot() +
  geom_point(mapping = aes(c(0:23),constricted_time_count))

constricted_time_count_18 = constrict_time(v_18_time_count)
ggplot() +
  geom_point(mapping = aes(c(0:23),constricted_time_count_18))

constricted_time_count_19 = constrict_time(v_19_time_count)
ggplot() +
  geom_point(mapping = aes(c(0:23),constricted_time_count_19))

constricted_time_count_20 = constrict_time(v_20_time_count)
ggplot() +
  geom_point(mapping = aes(c(0:23),constricted_time_count_20))

constricted_time_count_21 = constrict_time(v_21_time_count)
ggplot() +
  geom_point(mapping = aes(c(0:23),constricted_time_count_21))
