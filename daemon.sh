#!/bin/bash

# Archivo para almacenar la lista de archivos a eliminar
FILES_TO_DELETE="files_to_delete.txt"

# Función para encontrar y anotar nuevos archivos
find_new_files() {
    # Limpiar el archivo anterior
    > $FILES_TO_DELETE

    # Buscar archivos .ly en el directorio \output\
    find output/ -maxdepth 1 -type f -name "*.ly" >> $FILES_TO_DELETE

    # Buscar archivos .pdf en el directorio actual y subdirectorios
    find . -maxdepth 1 -type f -name "*.pdf" >> $FILES_TO_DELETE

    # Buscar archivos .mid en el directorio actual y subdirectorios
    find . -maxdepth 1 -type f -name "*.midi" >> $FILES_TO_DELETE
}

# Función para eliminar archivos anotados en la iteración anterior
delete_old_files() {
    if [ -f $FILES_TO_DELETE ]; then
        while IFS= read -r file; do
            if [ -f "$file" ]; then
                rm "$file"
            fi
        done < "$FILES_TO_DELETE"
    fi
}

# Proceso principal que se ejecuta cada 10 segundos
while true; do
    delete_old_files
    find_new_files
    sleep 10
done