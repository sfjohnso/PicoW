idf_component_register(SRCS "main.c" "settings.c" "portal.c" "font_data.c"
                       INCLUDE_DIRS "."
                       REQUIRES driver esp_driver_gpio esp_driver_spi esp_driver_i2c esp_lcd
                                esp_wifi esp_netif esp_event nvs_flash esp_partition
                                esp_http_client esp_http_server lwip mbedtls json esp_timer)

# installer/package.py sets this to build with placeholder settings instead of user_config.h
if(DEFINED ENV{P4CLOCK_INSTALLER_BUILD})
    target_compile_definitions(${COMPONENT_LIB} PRIVATE P4CLOCK_INSTALLER_BUILD)
endif()
