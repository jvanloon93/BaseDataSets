Monty <- function() {
  Q1 <- as.character(readline(prompt = "What is your quest?\r"))
  if(Q1 == "to seek the holy grail") {
    Q2 <- readline(prompt = "what is your favorite color?\r")
    if(Q2 == "blue") {
      Q3 <- readline(prompt = "What is the average airspeed velocity of an unlaiden swallow?\r")
      if(Q3 == "african or european?") {
        cat("I don't know that... ARGGGHH")
      } else {
        cat("ARGHHHHH ")
      }
    } else {
      cat("ARGGHHH")
    }
  }else {
    cat("ARGGGGHHH")
  }
}
