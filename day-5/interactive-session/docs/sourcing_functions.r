# Sourcing functions

# Create function for creating food cart name based on favorite food and animal
name_cart <- function(food, animal) {
  print(paste("Mc", stringr::str_to_title(animal), "'s ", stringr::str_to_title(food), "Mart", sep = ""))
}
name_cart("apples", "komodo dragon")
