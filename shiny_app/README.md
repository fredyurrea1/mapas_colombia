# Allianz Inteligencia Territorial

Aplicación Shiny para explorar la presencia territorial de Allianz frente al parque automotor, las cotizaciones, las emisiones y los clientes directos.

El mapa también incorpora las ventas de automóviles de enero de 2025 a marzo de 2026 y el mix de híbridos, eléctricos, gasolina y diésel. Estos indicadores están disponibles en los niveles departamental y regional, que es la granularidad de la fuente de ventas.

## Preparación inicial

Desde la raíz del proyecto, ejecute en PowerShell:

```powershell
& 'C:\Program Files\R\R-4.4.1\bin\Rscript.exe' shiny_app/install_dependencies.R
& 'C:\Program Files\R\R-4.4.1\bin\Rscript.exe' shiny_app/prepare_data.R
```

Las dependencias se instalan en la biblioteca estándar del usuario de R. La preparación:

- conserva intactos los archivos fuente;
- consolida las geometrías por código DANE;
- simplifica la geometría municipal para el navegador;
- normaliza nombres de departamento y municipio;
- calcula tasas por cada 1.000 automóviles;
- genera `shiny_app/data/territorial_data.rds`.

## Ejecutar

```powershell
& 'C:\Program Files\R\R-4.4.1\bin\Rscript.exe' shiny_app/run_app.R
```

Abra `http://127.0.0.1:3838`.

## Actualizar datos

Después de reemplazar cualquiera de las fuentes CSV o shapefiles, vuelva a ejecutar `prepare_data.R` y reinicie la aplicación.

## Alcance analítico

El índice de presencia es exploratorio. Combina intensidad de cotizaciones, penetración de pólizas directas y volumen absoluto de cotizaciones. No mide por sí solo recordación publicitaria ni causalidad.
