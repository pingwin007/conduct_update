# Black-Scholes Pricing Function
black_scholes <- function(S, K, T, r, sigma, type = "call") {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "call") {
    return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  } else if (type == "put") {
    return(K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1))
  } else {
    stop("Invalid option type. Use 'call' or 'put'.")
  }
}

# Main Engine Function
main <- function() {
  # Collect input values
  S <- as.numeric(readline("Enter current stock price (S): "))
  K <- as.numeric(readline("Enter strike price (K): "))
  T <- as.numeric(readline("Enter time to maturity in years (T): "))
  r <- as.numeric(readline("Enter risk-free interest rate (r): "))
  sigma <- as.numeric(readline("Enter volatility (sigma): "))
  type <- readline("Enter option type ('call' or 'put'): ")
  
  # Input validation
  if (any(is.na(c(S, K, T, r, sigma)))) {
    cat("Error: All inputs must be numeric values.\n")
    return(NULL)
  }
  
  # Calculate and display result
  tryCatch({
    price <- black_scholes(S, K, T, r, sigma, type)
    cat(sprintf("The %s option price is: %.4f\n", type, price))
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}

# Run the engine
if (interactive()) {
  main()
}