FROM swipl:latest

WORKDIR /app/pengines_server
COPY pengines_server/ ./

# Agregamos htpasswd y creamos el archivo de credenciales
RUN apt-get update && apt-get install -y --no-install-recommends apache2-utils \
 && htpasswd -bc passwd lcc lccdcic \
 && rm -rf /var/lib/apt/lists/*

EXPOSE 3030
CMD ["swipl", "run.pl"]
