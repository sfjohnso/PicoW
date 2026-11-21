idf_component_register(SRCS "main.c" "font_8x16.c"
                       INCLUDE_DIRS "."
                       REQUIRES esp_driver_spi esp_driver_gpio esp_driver_ledc esp_driver_i2c esp_driver_i2s esp_lcd esp_timer)
