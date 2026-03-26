# Zadanie 6 B block
ocena_kredytowa <- function(dochod, zadluzenie) {
  
  procent <- zadluzenie / dochod
  
  if (procent < 0.3) {
    return("KREDYT PRZYZNANY")
  } else if (procent <= 0.5) {
    return("WYMAGA WERYFIKACJI")
  } else {
    return("KREDYT ODRZUCONY")
  }
}

ocena_kredytowa(10000, 2000)
ocena_kredytowa(10000, 4000)
ocena_kredytowa(10000, 6000)