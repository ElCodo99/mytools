# Vorausgesetzt, die Klasse keepR ist bereits geladen und um die oben vorgeschlagenen Methoden erweitert.

# Instanz der keepR Klasse erstellen
my_keeper <- keepR$new(metric_names = c("R2", "RMSE"))

# Fünf Beispielmodelle mit zufälligen Metriken hinzufügen
model_names <- c("Modell_1", "Modell_2", "Modell_3", "Modell_4", "Modell_5")
metrics_list <- list(
  list(R2 = 0.9, RMSE = 5),
  list(R2 = 0.85, RMSE = 7),
  list(R2 = 0.92, RMSE = 4.5),
  list(R2 = 0.88, RMSE = 6),
  list(R2 = 0.91, RMSE = 5.5)
)

# Modelle mit add_batch hinzufügen
my_keeper$add_batch(model_names, metrics_list)

# Höchsten und niedrigsten R2-Wert abrufen
highest_R2 <- my_keeper$get_highest_metric("R2")
lowest_R2 <- my_keeper$get_lowest_metric("R2")
cat("Höchster R2-Wert:", highest_R2$value, "erzielt durch", highest_R2$model, "\n")
cat("Niedrigster R2-Wert:", lowest_R2$value, "erzielt durch", lowest_R2$model, "\n")

# Höchsten und niedrigsten RMSE-Wert abrufen
highest_RMSE <- my_keeper$get_highest_metric("RMSE")
lowest_RMSE <- my_keeper$get_lowest_metric("RMSE")
cat("Höchster RMSE-Wert:", highest_RMSE$value, "erzielt durch", highest_RMSE$model, "\n")
cat("Niedrigster RMSE-Wert:", lowest_RMSE$value, "erzielt durch", lowest_RMSE$model, "\n")
