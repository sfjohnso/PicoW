idf_component_register(SRCS "main.c" "font_data.c"
                       INCLUDE_DIRS "."
                       REQUIRES driver esp_driver_gpio esp_driver_spi esp_driver_i2c esp_lcd
                                esp_wifi esp_netif esp_event nvs_flash
                                esp_http_client mbedtls json esp_timer)
