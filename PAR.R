require(tidyverse)
require(magrittr)
require(here)

# Calibration
PAR1 <- here("PAR", "PAR1_Calibration.csv") %>%
  read_csv() %>%
  mutate(Datetime = dmy_hm(dateTime)) %>%
  rename(PAR = data1, Temp = data2) %T>%
  print()

PAR4 <- here("PAR", "PAR4_Calibration.csv") %>%
  read_csv() %>%
  mutate(Datetime = dmy_hm(dateTime)) %>%
  rename(PAR = data1, Temp = data2) %T>%
  print()

PAR5 <- here("PAR", "PAR5_Calibration.csv") %>%
  read_csv() %>%
  mutate(Datetime = dmy_hm(dateTime)) %>%
  rename(PAR = data1, Temp = data2) %T>%
  print()

ref <- here("PAR", "Reference.csv") %>%
  read_csv() %>%
  # Add 5 min because Timestamp records start time
  # while the logger records time after interval
  mutate(Datetime = Timestamp + dminutes(5)) %>%
  rename(PAR = `PAR (µmol/m²s)`) %T>%
  print()

ref %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

PAR1 %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

PAR4 %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

PAR5 %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

# Reference meter covered most of the light period,
# just need to truncate logger data at
ref %$% max(Datetime) # 16:25

PAR1 %<>% 
  filter(Datetime <= dmy_hm("11.10.26 16:25")) %T>%
  print()

PAR4 %<>% 
  filter(Datetime <= dmy_hm("11.10.26 16:25")) %T>%
  print()

PAR5 %<>% 
  filter(Datetime <= dmy_hm("11.10.26 16:25")) %T>%
  print()

ref %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

PAR1 %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

PAR4 %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

PAR5 %>%
  ggplot(aes(Datetime, PAR)) +
    geom_point() +
    theme_minimal()

# Looks good!

# The calibration on xpert.nz requires the average PAR
# from the reference meter, the sum of reset PAR from
# the logger, and the total calibration time.

# Mean
ref %$% mean(PAR) %>% print(digits = 18)
# 614.972446532987306 µmol photons m^-2 s^-1

# Total calibration time
# The actual start time was 5 min earlier than the first recorded value
ref %$% ( first(Datetime - dminutes(5)) %--% last(Datetime) / dminutes() ) 
# 595 min of total time over which PAR was measured

# Trapezoid integral time-averaged value
ref %$% diff(Datetime) %>% unique() # 5 min interval
ref %$% 
  ( 5 * ( sum(PAR) - 0.5 * sum( first(PAR) , last(PAR) ) ) / 595 ) %>%
  print(digits = 18)
# 614.307435167112885 µmol photons m^-2 s^-1

# Same calculation but coded differently:
ref %$% 
  ( 5/2 * sum( tail(PAR, -1) , head(PAR, -1) ) / 595 ) %>%
  print(digits = 18)

# Basically he same as the mean but I trust the trapezoid integral more.

# Sum of reset PAR for loggers
PAR1 %$% sum(PAR) # 1567956
PAR4 %$% sum(PAR) # 1571994
PAR5 %$% sum(PAR) # 1616051

# Check calibration
PAR1_c <- here("PAR", "PAR1_Calibrated.csv") %>%
  read_csv() %>%
  mutate(Datetime = dmy_hm(dateTime)) %>% 
  rename(PAR = data1, Temp = data2) %T>%
  print()

PAR4_c <- here("PAR", "PAR4_Calibrated.csv") %>%
  read_csv() %>%
  mutate(Datetime = dmy_hm(dateTime)) %>%
  rename(PAR = data1, Temp = data2) %T>%
  print()

PAR5_c <- here("PAR", "PAR5_Calibrated.csv") %>%
  read_csv() %>%
  mutate(Datetime = dmy_hm(dateTime)) %>%
  rename(PAR = data1, Temp = data2) %T>%
  print()

PAR1_c %>%
  full_join(ref, by = "Datetime", suffix = c("", "_ref")) %>%
  ggplot() +
    geom_point(aes(Datetime, PAR)) +
    geom_line(aes(Datetime, PAR_ref), colour = "darkorange") +
    theme_minimal()

PAR4_c %>%
  full_join(ref, by = "Datetime", suffix = c("", "_ref")) %>%
  ggplot() +
    geom_point(aes(Datetime, PAR)) +
    geom_line(aes(Datetime, PAR_ref), colour = "darkorange") +
    theme_minimal()
  
PAR5_c %>%
  full_join(ref, by = "Datetime", suffix = c("", "_ref")) %>%
  ggplot() +
    geom_point(aes(Datetime, PAR)) +
    geom_line(aes(Datetime, PAR_ref), colour = "darkorange") +
    theme_minimal()
# Looks pretty good! Logger 1 just seems to have missed that second spike.

