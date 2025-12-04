set.seed(123)

N <- 500

########################################
# 1. GENDER + REALISTIC NAMES
########################################

# Gender first
gender <- sample(c("Male", "Female"),
                 N, replace = TRUE,
                 prob = c(0.5, 0.5))

# Name lists by gender
male_first <- c(
  "Aaron","Adam","Adrian","Aiden","Alan","Albert","Alexander","Alfie","Andrew","Anthony",
  "Asher","Austin","Benjamin","Blake","Brandon","Brian","Bryan","Caleb","Cameron","Carlos",
  "Charles","Christian","Christopher","Cole","Colton","Connor","Cooper","Daniel","David",
  "Derek","Diego","Dominic","Dylan","Edward","Elijah","Elliot","Eric","Ethan","Evan",
  "Felix","Finn","Francis","Gabriel","Gavin","George","Grayson","Harrison","Harry","Henry",
  "Hudson","Hunter","Ian","Isaac","Isaiah","Jack","Jackson","Jacob","Jake","James",
  "Jason","Jasper","Jayden","Jeremiah","Jesse","John","Jonathan","Jordan","Joseph","Joshua",
  "Josiah","Juan","Julian","Justin","Kevin","Landon","Leo","Leon","Liam","Logan",
  "Luca","Lucas","Luis","Luke","Marcus","Mark","Mason","Matthew","Michael","Nathan",
  "Nicholas","Noah","Nolan","Oliver","Oscar","Owen","Patrick","Peter","Samuel","Sebastian"
)

female_first <- c(
  "Abigail","Addison","Aisha","Alexandra","Alice","Alicia","Amber","Amelia","Amy","Ana",
  "Andrea","Angela","Anna","Annie","Ariana","Arianna","Ashley","Aubrey","Ava","Beatrice",
  "Bella","Bethany","Brianna","Brooklyn","Camila","Carla","Caroline","Catherine","Charlotte","Chloe",
  "Christina","Clara","Daisy","Daniela","Danielle","Delilah","Diana","Eleanor","Elena","Eliana",
  "Elizabeth","Ella","Ellie","Emily","Emma","Erin","Eva","Evelyn","Faith","Fiona",
  "Florence","Freya","Gabriella","Georgia","Grace","Hailey","Hannah","Harper","Hazel","Isabel",
  "Isabella","Isla","Ivy","Jasmine","Jessica","Joanna","Josephine","Julia","Kaitlyn","Katherine",
  "Kayla","Khloe","Layla","Leah","Lillian","Lily","Lucy","Luna","Mackenzie","Madeline",
  "Madison","Margaret","Maria","Mary","Maya","Megan","Mia","Mila","Naomi","Natalie",
  "Natalia","Nora","Olivia","Paige","Penelope","Peyton","Rachel","Ruby","Sadie","Sophia"
)

# 100 popular last names (gender-neutral)
last_names <- c(
  "Smith","Johnson","Williams","Brown","Jones","Garcia","Miller","Davis","Rodriguez","Martinez",
  "Hernandez","Lopez","Gonzalez","Wilson","Anderson","Thomas","Taylor","Moore","Jackson","Martin",
  "Lee","Perez","Thompson","White","Harris","Sanchez","Clark","Ramirez","Lewis","Robinson",
  "Walker","Young","Allen","King","Wright","Scott","Torres","Nguyen","Hill","Flores",
  "Green","Adams","Nelson","Baker","Hall","Rivera","Campbell","Mitchell","Carter","Roberts",
  "Gomez","Phillips","Evans","Turner","Diaz","Parker","Cruz","Edwards","Collins","Reyes",
  "Stewart","Morris","Morales","Murphy","Cook","Rogers","Gutierrez","Ortiz","Morgan","Cooper",
  "Peterson","Bailey","Reed","Kelly","Howard","Ramos","Kim","Cox","Ward","Richardson",
  "Watson","Brooks","Chavez","Wood","James","Bennett","Gray","Mendoza","Ruiz","Hughes",
  "Price","Alvarez","Castillo","Sanders","Patel","Myers","Long","Ross","Foster","Jimenez"
)

# Choose first name consistent with gender
first_name <- character(N)
for (i in seq_len(N)) {
  if (gender[i] == "Male") {
    first_name[i] <- sample(male_first, 1)
  } else {
    first_name[i] <- sample(female_first, 1)
  }
}

last_name <- sample(last_names, N, replace = TRUE)
name <- paste(first_name, last_name)

# Simple ID
sample_id <- paste0("ID", sprintf("%04d", 1:N))

########################################
# 2. AGE, BMI, HEIGHT
########################################

age <- round(rnorm(N, mean = 45, sd = 15))
age[age < 18] <- sample(18:25, sum(age < 18), replace = TRUE)
age[age > 90] <- sample(75:90, sum(age > 90), replace = TRUE)

bmi <- round(rnorm(N, mean = 27, sd = 5), 1)
height <- round(rnorm(N, mean = 168, sd = 10), 1)

########################################
# 3. CITY–COUNTRY PAIRS (CONSISTENT)
########################################

# 5 countries, 10 cities (each city mapped to exactly one country)
city_levels <- c(
  "London","Manchester",        # UK
  "New York","Boston",          # USA
  "Berlin","Munich",            # Germany
  "Delhi","Mumbai",             # India
  "São Paulo","Rio de Janeiro"  # Brazil
)

country_by_city <- c(
  "UK","UK",
  "USA","USA",
  "Germany","Germany",
  "India","India",
  "Brazil","Brazil"
)
names(country_by_city) <- city_levels

# Ensure every city appears at least once
city <- c(city_levels, sample(city_levels, N - length(city_levels), replace = TRUE))
city <- sample(city, N, replace = FALSE)

country <- country_by_city[city]

########################################
# 4. EDUCATION LEVEL
########################################

education <- sample(
  c("Primary","High school","Bachelor","Master","PhD"),
  N, replace = TRUE,
  prob = c(0.1, 0.3, 0.35, 0.2, 0.05)
)

########################################
# 5. GENE EXPRESSION (10 VARS)
########################################

gene_expr <- replicate(10, rnorm(N, mean = 0, sd = 1))
colnames(gene_expr) <- paste0("gene_expr_", 1:10)

########################################
# 6. SNPS (5 VARS, 0/1/2)
########################################

snps <- replicate(5, sample(0:2, N, replace = TRUE, prob = c(0.7, 0.2, 0.1)))
colnames(snps) <- paste0("SNP_", 1:5)

########################################
# 7. CASE–CONTROL OUTCOME
########################################
# Logistic model using some of the other variables

logit <- -8 +
  0.04 * age +
  0.08 * bmi +
  ifelse(gender == "Male", 0.3,
         ifelse(gender == "Other", 0.1, 0)) +
  0.6 * (snps[, 1] == 2) +        # risk allele on SNP_1
  0.4 * (snps[, 2] == 2)          # risk allele on SNP_2

prob <- 1 / (1 + exp(-logit))
case_control <- rbinom(N, 1, prob)

########################################
# 8. FINAL DATA FRAME
########################################

df <- data.frame(
  name,
  sample_id,
  age,
  gender,
  bmi,
  height,
  country,
  city,
  education,
  gene_expr,
  snps,
  case_control
)

head(df)
