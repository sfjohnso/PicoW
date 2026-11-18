idf_component_register(SRCS "main.c" "font_8x16.c"
                       INCLUDE_DIRS "."
                       REQUIRES esp_driver_spi esp_driver_gpio esp_driver_ledc esp_driver_sdmmc esp_driver_i2c
                                esp_lcd fatfs sdmmc esp_timer esp_psram esp_driver_jpeg)
