
setlocal enabledelayedexpansion

:: Archivo para almacenar la lista de archivos a eliminar
set "FILES_TO_DELETE=files_to_delete.txt"

:main_loop
    call :delete_old_files
    call :find_new_files
    timeout /t 10 /nobreak >nul
    goto main_loop
exit /b

:delete_old_files
    if exist "%FILES_TO_DELETE%" (
        for /f "delims=" %%a in (%FILES_TO_DELETE%) do (
            if exist "%%a" (
                del "%%a"
            )
        )
    )
    exit /b

:find_new_files
    (echo. > "%FILES_TO_DELETE%")

    :: Buscar archivos .ly en el directorio \output\
    for /r "output\" %%i in (*.ly) do (
        echo %%i >> "%FILES_TO_DELETE%"
    )

    :: Buscar archivos .pdf en el directorio actual y subdirectorios
    for /r . %%i in (*.pdf) do (
        echo %%i >> "%FILES_TO_DELETE%"
    )

    :: Buscar archivos .mid en el directorio actual y subdirectorios
    for /r . %%i in (*.mid) do (
        echo %%i >> "%FILES_TO_DELETE%"
    )

    exit /b
