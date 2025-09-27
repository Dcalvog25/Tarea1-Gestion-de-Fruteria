
use "auxiliares.sml";


(* agregarRegistro
   Entrada: unit
   Salida: unit
   Objetivo: Agregar nuevo registro de fruta al catálogo CSV *)
fun agregarRegistro () = 
    let 
        val _ = mostrarTitulo "AGREGAR REGISTRO DE FRUTA"
        val nombreArchivo = "catalogo_frutas.csv"
        val encabezado = "codigo,nombre,familia,cantidad_vendida,precio_unitario\n"
        
        (* Crear archivo si no existe *)
        val _ = crearArchivoSiNoExiste nombreArchivo encabezado
        
        (* Solicitar datos al usuario *)
        val codigo = leerEntrada "Ingrese el codigo de la fruta (ej: FR001): "
        val nombre = leerEntrada "Ingrese el nombre de la fruta: "
        val familia = leerEntrada "Ingrese la familia de la fruta: "
        val cantidad = leerEntero "Ingrese la cantidad vendida: "
        val precio = leerReal "Ingrese el precio unitario: "
        
        (* Crear la línea CSV *)
        val campos = [codigo, nombre, familia, Int.toString(cantidad), Real.toString(precio)]
        val lineaCSV = unirStrings "," campos ^ "\n"
        
        (* Escribir al archivo *)
        val archivoSalida = TextIO.openAppend nombreArchivo
        val _ = TextIO.output(archivoSalida, lineaCSV)
        val _ = TextIO.closeOut archivoSalida
        
        (* Mostrar confirmación *)
        val _ = mostrarTitulo "REGISTRO AGREGADO"
        val _ = print("Codigo: " ^ codigo ^ "\n")
        val _ = print("Nombre: " ^ nombre ^ "\n") 
        val _ = print("Familia: " ^ familia ^ "\n")
        val _ = print("Cantidad vendida: " ^ Int.toString(cantidad) ^ "\n")
        val _ = print("Precio unitario: " ^ Real.toString(precio) ^ "\n")
        val _ = print("Registro guardado en " ^ nombreArchivo ^ "\n")
        val _ = pausar()
    in
        ()
    end


(* limpiarCatalogo
   Entrada: unit
   Salida: unit
   Objetivo: Limpiar todo el contenido del catálogo con confirmación *)
fun limpiarCatalogo () =
    let 
        val mensaje = "¿Está seguro de que desea limpiar todo el catálogo?"
    in
        if confirmarAccion mensaje then
            let 
                val archivo = TextIO.openOut "catalogo_frutas.csv"
                val _ = TextIO.output(archivo, "codigo,nombre,familia,cantidad_vendida,precio_unitario\n")
                val _ = TextIO.closeOut archivo
                val _ = print("Catalogo limpiado exitosamente.\n")
                val _ = pausar()
            in
                ()
            end
        else
            let
                val _ = print("Operacion cancelada.\n")
                val _ = pausar()
            in
                ()
            end
    end

(* menuAdministracionCatalogo
   Entrada: unit
   Salida: unit
   Objetivo: Mostrar menú de administración y manejar opciones *)
fun menuAdministracionCatalogo () =
    let 
        val _ = mostrarTitulo "ADMINISTRACION DE CATALOGOS"
        val _ = print("1. Agregar Registro\n")
        val _ = print("2. Limpiar Catalogo\n")
        val _ = print("3. Volver al menu principal\n")
        val option = leerEntrada "Seleccione una opcion: "
    in
        case option of
            "1" => 
                let 
                    val _ = agregarRegistro()
                in
                    menuAdministracionCatalogo()
                end
          | "2" =>
                let 
                    val _ = limpiarCatalogo()
                in
                    menuAdministracionCatalogo()    
                end
          | "3" =>
                let 
                    val _ = print("Volviendo al menu principal...\n")
                in
                    ()
                end
          | _ =>
                let
                    val _ = print("Opcion no valida. Por favor, intente de nuevo.\n")
                    val _ = pausar()
                in
                    menuAdministracionCatalogo()
                end
    end
