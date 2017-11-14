log_config <- rjson::fromJSON(file = "data-raw/log_config.json")
devtools::use_data(log_config)
