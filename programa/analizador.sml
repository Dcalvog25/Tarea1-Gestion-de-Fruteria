use "auxiliares.sml";


(* Función para buscar frutas por código o nombre *)
fun buscarFrutasPorCodigoONombre archivo =
    let
        val _ = mostrarTitulo "BUSCAR FRUTAS POR CODIGO O NOMBRE"
        val terminoBusqueda = leerEntrada "Ingrese el codigo o nombre de la fruta a buscar: "
        
        (* Leer todos los registros del archivo *)
        val registros = leerRegistrosCSV archivo
        
        (* Filtrar registros que coincidan con el código o nombre *)
        val registrosEncontrados = List.filter (fn (codigo, nombre, _, _, _) =>
            contieneCadena terminoBusqueda codigo orelse 
            contieneCadena terminoBusqueda nombre
        ) registros
        
        (* Mostrar resultados *)
        val _ = print("\n=== RESULTADOS DE BUSQUEDA ===\n")
        val _ = print("Termino de busqueda: \"" ^ terminoBusqueda ^ "\"\n")
        val _ = print("Archivo: " ^ archivo ^ "\n\n")
        
    in
        case registrosEncontrados of
            [] => print("No se encontraron frutas que coincidan con \"" ^ terminoBusqueda ^ "\".\n")
          | _ =>
                let
                    val numResultados = List.length registrosEncontrados
                    val _ = print("Se encontraron " ^ Int.toString(numResultados) ^ " registro(s):\n\n")
                    
                    
                    val _ = List.app mostrarRegistroFruta registrosEncontrados
                    
                    
                    val (totalUnidades, totalVentas) = List.foldl (fn ((_, _, _, cantidad, precio), (accUnidades, accVentas)) =>
                        (accUnidades + cantidad, accVentas + (Real.fromInt(cantidad) * precio))
                    ) (0, 0.0) registrosEncontrados
                    
                    val _ = print("\n=== RESUMEN ===\n")
                    val _ = print("Total de registros encontrados: " ^ Int.toString(numResultados) ^ "\n")
                    val _ = print("Total de unidades vendidas: " ^ Int.toString(totalUnidades) ^ "\n")
                    val _ = print("Total de ventas: $" ^ Real.toString(totalVentas) ^ "\n")
                    
                    (* Mostrar nombres únicos usando map para extraer nombres *)
                    val nombres = List.map (fn (_, nombre, _, _, _) => nombre) registrosEncontrados
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val nombresUnicos = eliminarDuplicados nombres
                    val _ = print("Frutas diferentes encontradas: " ^ unirStrings ", " nombresUnicos ^ "\n")
                in
                    ()
                end
    end


fun analizarFrutasPopulares archivo =
    print("Funcion 'Mostrar frutas populares' no implementada aun.\n")

fun analizarFamiliasConMuchasFrutas archivo =
    print("Funcion 'Identificar familias con mas de 4 frutas' no implementada aun.\n")

fun contarFrutasPorFamilia archivo =
    print("Funcion 'Cantidad de frutas por familia' no implementada aun.\n")

fun resumenGeneralVerduleria archivo =
    print("Funcion 'Resumen general de la verdulería' no implementada aun.\n")

fun volverMenuPrincipal () =
    print("Volviendo al menu principal...\n")

fun menuOpcionesAnalisis archivo =
    let
        val _ = print("Seleccione una opcion de analisis para el archivo " ^ archivo ^ ":\n")
        val _ = print("1. Mostrar frutas populares dentro de un rango de cantidad vendida\n")
        val _ = print("2. Identificar familias con mas de 4 frutas diferentes registradas\n")
        val _ = print("3. Buscar frutas por código o nombre\n")
        val _ = print("4. Cantidad de frutas por familia\n")
        val _ = print("5. Resumen general de la verdulería\n")
        val _ = print("6. Volver al menu principal\n")
        val opcion = leerEntrada "Opcion: "
    in
        case opcion of
            "1" => 
                let 
                    val _ = analizarFrutasPopulares archivo
                    val _ = pausar()
                in
                    menuOpcionesAnalisis archivo  
                end
            | "2" =>
                let 
                    val _ = analizarFamiliasConMuchasFrutas archivo
                    val _ = pausar()
                in
                    menuOpcionesAnalisis archivo  
                end
            | "3" =>
                let
                    val _ = buscarFrutasPorCodigoONombre archivo
                    val _ = pausar()
                in
                    menuOpcionesAnalisis archivo
                end
            | "4" =>
                let
                    val _ = contarFrutasPorFamilia archivo
                    val _ = pausar()
                in
                    menuOpcionesAnalisis archivo
                end
            | "5" =>
                let
                    val _ = resumenGeneralVerduleria archivo
                    val _ = pausar()
                in
                    menuOpcionesAnalisis archivo
                end
            | "6" =>
                let
                    val _ = volverMenuPrincipal()
                in
                    ()
                end
    end

fun menuAnalisisRegistros () =
    let
        val _ = mostrarTitulo "Bienvenido al Analizador de Registros"
        val _ = print("Ingrese la ruta del archivo que desea analizar: \n")
        val archivo = leerEntrada "Archivo: "
        val _ =
            if not (archivoExiste archivo) then
                print("El archivo no existe. Por favor, verifique la ruta e intente de nuevo.\n")
            else
                menuOpcionesAnalisis archivo
    in
        ()
    end