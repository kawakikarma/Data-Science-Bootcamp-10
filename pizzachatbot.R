# Load necessary library
library(dplyr)

# Pizza menu
pizza_data <- data.frame(
  pizza_id = 1:8,
  pizza_menu = c(
    "Extreme Ham & Cheese Crust",
    "Super Value",
    "Chessy Puff",
    "Pan",
    "Crispy Thin",
    "Extreme Crust",
    "New York Pizza Max 9 Inches",
    "New York Pizza Mega 12 Inches"
  ),
  pizza_price = c(399, 129, 149, 129, 129, 419, 299, 399)
)

# Drink menu
drink_data <- data.frame(
  drink_id = 1:6,
  drink_menu = c("Coke", "Fanta", "Pepsi", "Est", "Lemonade", "Water"),
  drink_price = c(40, 40, 40, 40, 35, 20)
)

# Show pizza menu
show_pizza_menu <- function() {
  cat("\n**Pizza Menu**\n")
  print(pizza_data)
}

# Show drink menu
show_drink_menu <- function() {
  cat("\n**Drink Menu**\n")
  print(drink_data)
}

# Get pizza order
get_pizza_order <- function() {
  show_pizza_menu()
  repeat {
    pizza_choice <- readline("Enter the pizza ID you want to order (or 'q' to quit): ")
    
    if (tolower(pizza_choice) == "q") {
      return(NULL)
    }
    
    pizza_id <- as.numeric(pizza_choice)
    if (!is.na(pizza_id) && pizza_id %in% pizza_data$pizza_id) {
      pizza_quantity <- as.numeric(readline("How many pizzas would you like (enter a number): "))
      if (!is.na(pizza_quantity) && pizza_quantity > 0) {
        return(data.frame(pizza_order = pizza_id, pizza_quantity = pizza_quantity))
      } else {
        cat("Invalid quantity. Please enter a positive number.\n")
      }
    } else {
      cat("Invalid pizza ID. Please enter a valid ID.\n")
    }
  }
}

# Get drink order
get_drink_order <- function() {
  show_drink_menu()
  repeat {
    drink_choice <- readline("Enter the drink ID you want to order (or 'q' to quit): ")
    
    if (tolower(drink_choice) == "q") {
      return(NULL)
    }
    
    drink_id <- as.numeric(drink_choice)
    if (!is.na(drink_id) && drink_id %in% drink_data$drink_id) {
      drink_quantity <- as.numeric(readline("How many drinks would you like (enter a number): "))
      if (!is.na(drink_quantity) && drink_quantity > 0) {
        return(data.frame(drink_order = drink_id, drink_quantity = drink_quantity))
      } else {
        cat("Invalid quantity. Please enter a positive number.\n")
      }
    } else {
      cat("Invalid drink ID. Please enter a valid ID.\n")
    }
  }
}

# Calculate total
calculate_total <- function(pizza_order, drink_order) {
  pizza_total <- if (!is.null(pizza_order)) {
    pizza_order %>%
      mutate(total_price = pizza_quantity * pizza_data$pizza_price[pizza_order]) %>%
      pull(total_price) %>%
      sum()
  }
  drink_total <- if (!is.null(drink_order)) {
    drink_order %>%
      mutate(total_price = drink_quantity * drink_data$drink_price[drink_order]) %>%
      pull(total_price) %>%
      sum()
  }
  
  return(pizza_total + drink_total)
}

# Main function
pizza_order <- get_pizza_order()
  if (is.null(pizza_order)) {
    cat("Thank you for visiting AutoBot Pizza!\n")
    return()
  }
  
  
  total_price <- calculate_total(pizza_order, drink_order)
  
  cat("\n**Your Order**\n")
  print(pizza_order)
  if (!is.null(drink_order)) {
    print(drink_order)
  }
  cat("\nTotal price:", total_price, "\n")
  
  cat("Thank you for your order!\n")

