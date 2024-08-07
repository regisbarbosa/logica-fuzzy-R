### Step 1: Installation and Loading of Packages

install.packages("FuzzyR")
install.packages("readxl")

library(readxl)
library(FuzzyR)


### Importing the data

route_data <- read_excel("dados_percursos.xlsx", sheet = "Dados")
route_data
traffic_lights_data <- read_excel("dados_semaforo.xlsx", sheet = "Dados")
traffic_lights_data


# Step 2: Create the Fuzzy System

green_control <- newfis("Smart Green Control")

# Step 3: Add Input and Output Variables

# Add input variables
green_control <- addvar(green_control, "input", "Intensity", c(0, 40))
green_control <- addvar(green_control, "input", "Travel_Time", c(0, 11))

# Add output variable
green_control <- addvar(green_control, "output", "Green", c(25, 60))


# Step 4: Define Membership Functions

### Membership functions for "Intensity"

green_control <- addmf(green_control, "input", 1, "Low", "trimf", c(0, 10, 20))
green_control <- addmf(green_control, "input", 1, "Medium", "trimf", c(15, 25, 35))
green_control <- addmf(green_control, "input", 1, "High", "trimf", c(30, 40, 40))


######## Membership functions for "Travel_Time"

green_control <- addmf(green_control, "input", 2, "Short", "trimf", c(0, 6, 6.55)) # up to 6.55 minutes
green_control <- addmf(green_control, "input", 2, "Medium", "trimf", c(7, 8, 8.5)) # from 7 to 8.5 minutes
green_control <- addmf(green_control, "input", 2, "Long", "trimf", c(9, 10, 11)) # from 9 to 11 minutes

####### Membership functions for "Green"

green_control <- addmf(green_control, "output", 1, "Short", "trimf", c(30, 35, 35))
green_control <- addmf(green_control, "output", 1, "Medium", "trimf", c(40, 50, 60))



# Step 5: Define the Rule Base

ruleListGreen <- rbind(
  c(1, 1, 1, 1, 1), # If Intensity is Low and Travel_Time is Short, then Green is Short
  c(1, 2, 1, 1, 1), # If Intensity is Low and Travel_Time is Medium, then Green is Short
  c(1, 3, 1, 1, 1), # If Intensity is Low and Travel_Time is Long, then Green is Short
  c(2, 1, 2, 1, 1), # If Intensity is Medium and Travel_Time is Short, then Green is Medium
  c(2, 2, 2, 1, 1), # If Intensity is Medium and Travel_Time is Medium, then Green is Medium
  c(2, 3, 2, 1, 1), # If Intensity is Medium and Travel_Time is Long, then Green is Long
  c(3, 3, 2, 1, 1)  # If Intensity is High and Travel_Time is Long, then Green is Medium
)

green_control <- addrule(green_control, ruleListGreen)
green_control

# Step 6: Test the Fuzzy System

green_input <- as.matrix(route_data[, c("Intensidade", "Tempo")])
green_input
str(green_input)

# Evaluate the fuzzy system
green_output <- evalfis(green_input, green_control)
green_output

# Rounding

green_output <- round(green_output, 0)
green_output

##### Red Light

# Step 1: Create the Fuzzy System

red_control <- newfis("Smart Red Control")

### Step 2: Add Input and Output Variables (Same as Green Light)

# Add input variables
red_control <- addvar(red_control, "input", "Intensity", c(0, 40))
red_control <- addvar(red_control, "input", "Travel_Time", c(0, 11))

# Add output variable for Red
red_control <- addvar(red_control, "output", "Red", c(60, 70))

### Step 3: Define Membership Functions 
# Membership functions for Intensity and Travel Time are the same as Green

# Membership functions for "Intensity"
red_control <- addmf(red_control, "input", 1, "Low", "trimf", c(0, 10, 20))
red_control <- addmf(red_control, "input", 1, "Medium", "trimf", c(15, 25, 35))
red_control <- addmf(red_control, "input", 1, "High", "trimf", c(30, 40, 40))

# Membership functions for "Travel_Time"
red_control <- addmf(red_control, "input", 2, "Short", "trimf", c(0, 6, 6.55)) # up to 6.55 minutes
red_control <- addmf(red_control, "input", 2, "Medium", "trimf", c(7, 8, 8.5)) # from 7 to 8.5 minutes
red_control <- addmf(red_control, "input", 2, "Long", "trimf", c(9, 10, 11)) # from 9 to 11 minutes

# Membership functions for "Red" 

red_control <- addmf(red_control, "output", 1, "Medium", "trimf", c(60, 65, 70))
red_control <- addmf(red_control, "output", 1, "Long", "trimf", c(65, 70, 75))

### Step 4: Define the Rule Base

# Rules for Red
ruleListRed <- rbind(
  c(1, 1, 2, 1, 1), # If Intensity is Low and Travel_Time is Short, then Red is Long
  c(1, 2, 1, 1, 1), # If Intensity is Low and Travel_Time is Medium, then Red is Medium
  c(1, 3, 2, 1, 1), # If Intensity is Low and Travel_Time is Long, then Red is Long
  c(2, 2, 1, 1, 1), # If Intensity is Medium and Travel_Time is Medium, then Red is Medium
  c(2, 3, 2, 1, 1), # If Intensity is Medium and Travel_Time is Long, then Red is Long
  c(3, 2, 1, 1, 1), # If Intensity is High and Travel_Time is Medium, then Red is Medium
  c(3, 3, 2, 1, 1)  # If Intensity is High and Travel_Time is Long, then Red is Long
)

# Adding the adjusted rules for Red
red_control <- addrule(red_control, ruleListRed)
red_control

### Step 5: Test the Fuzzy System

red_input <- as.matrix(route_data[, c("Intensidade", "Tempo")])
red_input
str(red_input)

# Evaluate the fuzzy system
red_output <- evalfis(red_input, red_control)
red_output

# Rounding the Red light value
red_output <- round(red_output, 0)
red_output

# Summary Table
# First, add the output to the input matrix
data <- cbind(green_input, Green = green_output, Red = red_output)
data

complete_data_df <- as.data.frame(data)
colnames(complete_data_df) <- c("Intensity", "Travel Time", "Green Fuzzy", "Red Fuzzy")
complete_data_df
