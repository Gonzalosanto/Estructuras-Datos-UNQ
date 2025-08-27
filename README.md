# Practicas de Estructura de Datos - UNQ
<h3> No oficial - Hecho por estudiantes </h3>

<h3> Ante dudas puntuales con algun ejercicio consultar con los docentes. </h3>

Repositorio para recopilar las soluciones de practicas de la materia EstrDatos de la Universidad Nacional de Quilmes.
No se deben tomar las soluciones como definitivas, si no, como sugerencias. Dichas propuestas no reemplazan a las soluciones 
explicadas por los docentes. 

## Instrucciones para instalar GHCI

Fuente: [Haskell](https://www.haskell.org/ghcup/install/#how-to-install)

### Linux, macOS, FreeBSD o Windows Subsystem 2 for Linux:

Ejecutar en la terminal:

``` curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh ```

### Windows:

Ejecutar en la terminal:

```
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```
## Para ejecutar el codigo

En la carpeta donde se encuentra el codigo, abrir la terminal y ejecutar el comando `ghci archivo-de-practica.hs` Reemplazando el nombre por el que se uso para guardar el archivo.
