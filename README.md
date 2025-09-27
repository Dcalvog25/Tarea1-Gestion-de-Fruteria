# Tarea2-Gestion-de-Fruteria

## Información del Curso
- **Nombre del curso:** Lenguajes de Programacion
- **Número de semestre y año lectivo:** IIS 2025
- **Nombre del Estudiante:** David Calvo García
- **Número de carnet:** 2024122451
- **Número de tarea:** Tarea #2
- **Fecha de entrega:** 26 de septiembre del 2025
- **Estatus de la entrega:** Excelente

## Descripción
Sistema de gestión de frutería desarrollado en Standard ML (SML) que permite administrar catálogos de frutas y realizar análisis estadísticos sobre registros de ventas.

## Funcionalidades

### 1. Administración de Catálogos
- **Agregar Registro:** Permite agregar nuevos registros de frutas al catálogo CSV
- **Limpiar Catálogo:** Elimina todos los registros del catálogo (con confirmación)

### 2. Análisis de Registros
- **Buscar frutas por código o nombre:** Búsqueda flexible con coincidencias parciales
- **Frutas populares por rango de monto:** Ranking de frutas dentro de un rango específico de ventas
- **Familias con muchas frutas:** Identifica familias con más de 4 frutas diferentes
- **Frutas por familia:** Lista y cuenta frutas de una familia específica
- **Resumen general:** Estadísticas completas de la verdulería

## Estructura del Proyecto
```
programa/
├── main.sml           # Función principal y menú principal
├── auxiliares.sml     # Funciones utilitarias y validación
├── creador.sml        # Módulo de administración de catálogos
├── analizador.sml     # Módulo de análisis
```

## Cómo ejecutar
1. Abrir intérprete de SML
2. Cargar el programa: `use "main.sml";`
3. El programa iniciará automáticamente mostrando el menú principal

## Formato de datos CSV
```
codigo,nombre,familia,cantidad_vendida,precio_unitario
FR001,Piña,Bromeliaceae,150,500
```

## Características técnicas
- **Lenguaje:** Standard ML (SML)
- **Paradigma:** Programación funcional
- **Persistencia:** Archivos CSV de texto plano
- **Validación:** Entrada de datos con manejo de errores
- **Funciones de orden superior:** Uso extensivo de `List.filter`, `List.map`, `List.foldl`