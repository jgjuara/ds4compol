# iniciamos el driver:
# load RSelenium
library(RSelenium)
# start the Selenium server (in docker)
system("docker run -d -p 4445:4444 selenium/standalone-firefox")
# initiate the Selenium session
myclient <- remoteDriver(#remoteServerAddr = "localhost",
                         port = 4445L,
                         browserName = "firefox")
# start browser session
myclient$open()


# el driver tiene dentro server y cliente
# ahora se nos debería haber abierto una sesion de navegador

# el objeto creado tiene funciones propias
# preguntar estado
driver$client$getStatus() 

# cerrar la sesión
driver$client$close()

#abre una sesión en navegador
driver$client$open()