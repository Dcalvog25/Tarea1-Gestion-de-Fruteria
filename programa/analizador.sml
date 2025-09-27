use "auxiliares.sml";


(* buscarFrutasPorCodigoONombre
   Entrada: string (archivo)
   Salida: unit
   Objetivo: Buscar frutas por código o nombre y mostrar resultados *)
fun buscarFrutasPorCodigoONombre archivo =
    let
        val _ = mostrarTitulo "BUSCAR FRUTAS POR CODIGO O NOMBRE"
        val terminoBusqueda = leerEntrada "Ingrese el codigo o nombre de la fruta a buscar: "
        
        (* Leer todos los registros del archivo *)
        val registros = leerRegistrosCSV archivo
        
        
        (* Filtrar registros que coincidan con el código o nombre *)
        val registrosEncontrados = List.filter (fn (codigo, nombre, _, _, _) =>
            terminoBusqueda = codigo orelse 
            terminoBusqueda = nombre
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


(* analizarFrutasPopulares
   Entrada: string (archivo)
   Salida: unit
   Objetivo: Mostrar ranking de frutas populares en rango de monto *)
fun analizarFrutasPopulares archivo =
    let
        val _ = mostrarTitulo "FRUTAS POPULARES DENTRO DE UN RANGO DE MONTO VENDIDO"
        val montominimo = leerReal "Ingrese el monto minimo vendido: $"
        val montomaximo = leerReal "Ingrese el monto maximo vendido: $"

        val registros = leerRegistrosCSV archivo

        (* Función para agrupar frutas por nombre y sumar sus ventas *)
        fun agruparFrutasPorNombre [] = []
          | agruparFrutasPorNombre registros =
                let

                    val nombres = List.map (fn (_, nombre, _, _, _) => nombre) registros
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val nombresUnicos = eliminarDuplicados nombres

                    (* Para cada nombre único, calcular totales *)
                    fun calcularTotalesPorNombre nombre =
                        let
                            val registrosFruta = List.filter (fn (_, n, _, _, _) => n = nombre) registros
                            val (codigo, _, familia, _, _) = hd registrosFruta (* Tomar datos del primer registro *)
                            val (totalUnidades, totalVentas) = 
                                List.foldl (fn ((_, _, _, cantidad, precio), (accUnidades, accVentas)) =>
                                    (accUnidades + cantidad, accVentas + (Real.fromInt(cantidad) * precio))
                                ) (0, 0.0) registrosFruta
                        in
                            (codigo, nombre, familia, totalUnidades, totalVentas)
                        end
                in
                    List.map calcularTotalesPorNombre nombresUnicos
                end

        (* Agrupar frutas *)
        val frutasAgrupadas = agruparFrutasPorNombre registros

        (* Filtrar frutas dentro del rango *)
        val frutasEnRango = List.filter (fn (_, _, _, _, totalVentas) =>
            totalVentas >= montominimo andalso totalVentas <= montomaximo
        ) frutasAgrupadas

        (* Ordenar por monto total vendido en orden descendente *)
        fun ordenarPorMontoDesc [] = []
          | ordenarPorMontoDesc [x] = [x]
          | ordenarPorMontoDesc lista =
              let
                  fun ordenar [] = []
                    | ordenar [x] = [x]
                    | ordenar ((x as (_, _, _, _, montoX)) :: (y as (_, _, _, _, montoY)) :: resto) =
                        if montoX >= montoY then
                            x :: ordenar (y :: resto)
                        else
                            y :: ordenar (x :: resto)

                  fun ordenamiento lista 0 = lista
                    | ordenamiento lista n = ordenamiento (ordenar lista) (n - 1)
              in
                  ordenamiento lista (List.length lista)
              end

        val frutasOrdenadas = ordenarPorMontoDesc frutasEnRango

        (* Función para mostrar fruta agrupada *)
        fun mostrarFrutaAgrupada (codigo, nombre, familia, totalUnidades, totalVentas) =
            let
                val preciounitario = if totalUnidades > 0 then totalVentas / Real.fromInt(totalUnidades) else 0.0
            in
                print("     Nombre: " ^ nombre ^ "\n");
                print("     Codigo: " ^ codigo ^ "\n");
                print("     Familia: " ^ familia ^ "\n");
                print("     Total unidades vendidas: " ^ Int.toString(totalUnidades) ^ "\n");
                print("     MONTO TOTAL VENDIDO: $" ^ Real.toString(totalVentas) ^ "\n");
                print("     PRECIO UNITARIO: $" ^ Real.toString(preciounitario) ^ "\n");
                print("     " ^ String.implode (List.tabulate(50, fn _ => #"-")) ^ "\n")
            end

        (* Mostrar resultados *)
        val _ = print("\n=== RANKING DE FRUTAS POPULARES EN EL RANGO DE $" ^ Real.toString(montominimo) ^ " A $" ^ Real.toString(montomaximo) ^ " ===\n")
        val _ = 
            case frutasOrdenadas of
                [] => print("No se encontraron frutas en el rango especificado.\n")
              | _ => 
                    let
                        val numResultados = List.length frutasOrdenadas
                        val _ = print("Se encontraron " ^ Int.toString(numResultados) ^ " tipos de fruta en el rango:\n\n")
                        
                        val _ = List.app mostrarFrutaAgrupada frutasOrdenadas
                        
                        (* Mostrar resumen *)
                        val totalVentasEnRango = List.foldl (fn ((_, _, _, _, ventas), acc) => acc + ventas) 0.0 frutasOrdenadas
                        val totalUnidadesEnRango = List.foldl (fn ((_, _, _, unidades, _), acc) => acc + unidades) 0 frutasOrdenadas
                        
                        val _ = print("\n=== RESUMEN DEL RANGO ===\n")
                        val _ = print("Total de tipos de fruta en el rango: " ^ Int.toString(numResultados) ^ "\n")
                        val _ = print("Total de unidades vendidas en el rango: " ^ Int.toString(totalUnidadesEnRango) ^ "\n")
                        val _ = print("Total de ventas en el rango: $" ^ Real.toString(totalVentasEnRango) ^ "\n")
                    in
                        ()
                    end
    in
        ()
    end

(* analizarFamiliasConMuchasFrutas
   Entrada: string (archivo)
   Salida: unit
   Objetivo: Identificar familias con más de 4 frutas diferentes *)
fun analizarFamiliasConMuchasFrutas archivo =
    let
        val _ = mostrarTitulo "FAMILIAS CON MAS DE 4 FRUTAS DIFERENTES REGISTRADAS"
        val registros = leerRegistrosCSV archivo

        fun agruparPorFamilia [] = []
          | agruparPorFamilia registros =
                let
                    val familias = List.map (fn (_, _, familia, _, _) => familia) registros
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val familiasUnicas = eliminarDuplicados familias

                    fun contarFrutasEnFamilia familia =
                        let
                            val frutasEnFamilia = List.filter (fn (_, _, f, _, _) => f = familia) registros
                            val nombres = List.map (fn (_, nombre, _, _, _) => nombre) frutasEnFamilia
                            fun eliminarDuplicados [] = []
                              | eliminarDuplicados (x::xs) = 
                                    x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                            val nombresUnicos = eliminarDuplicados nombres
                        in
                            (familia, List.length nombresUnicos)
                        end
                in
                    List.map contarFrutasEnFamilia familiasUnicas
                end
        val conteoPorFamilia = agruparPorFamilia registros
        val familiasConMuchasFrutas = List.filter (fn (_, count) => count >= 4) conteoPorFamilia
        val _ = print("\n=== FAMILIAS CON MAS DE 4 FRUTAS DIFERENTES ===\n")
        val _ = 
            case familiasConMuchasFrutas of
                [] => print("No se encontraron familias con mas de 4 frutas diferentes.\n")
              | _ =>
                    let
                        val numFamilias = List.length familiasConMuchasFrutas
                        val _ = print("Se encontraron " ^ Int.toString(numFamilias) ^ " familia(s) con mas de 4 frutas diferentes:\n\n")
                        val _ = List.app (fn (familia, count) =>
                            print("  Familia: " ^ familia ^ " - Cantidad de frutas diferentes: " ^ Int.toString(count) ^ "\n")
                        ) familiasConMuchasFrutas
                    in
                        ()
                    end
    in
        ()
    end 

(* contarFrutasPorFamilia
   Entrada: string (archivo)
   Salida: unit
   Objetivo: Contar y listar frutas de una familia específica *)
fun contarFrutasPorFamilia archivo =
    let 
        val _ = mostrarTitulo "CANTIDAD Y LISTA DE FRUTAS POR FAMILIA"
        val registros = leerRegistrosCSV archivo

        val busqueda = leerEntrada "Ingrese el nombre de la familia a buscar: "
        val registroFamilia = List.filter (fn(_,_,familia,_,_) => busqueda = familia) registros
    in
        case registroFamilia of
            [] => print("No se encontraron frutas para la familia: " ^ busqueda ^ "\n")
          | _ =>
                let
                    val _ = print("Frutas encontradas para la familia: " ^ busqueda ^ "\n")
                
                    val nombres = List.map (fn (_, nombre, _, _, _) => nombre) registroFamilia
                    
                    (* Eliminar nombres duplicados *)
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val nombresUnicos = eliminarDuplicados nombres
                    
                    (* Mostrar la lista de nombres únicos *)
                    val _ = List.app (fn nombre => print(" - " ^ nombre ^ "\n")) nombresUnicos
                    val _ = print("Total de frutas en la familia " ^ busqueda ^ ": " ^ Int.toString(List.length nombresUnicos) ^ "\n")
                in
                    ()
                end
    end

(* resumenGeneralVerduleria
   Entrada: string (archivo)
   Salida: unit
   Objetivo: Generar resumen completo con estadísticas generales *)
fun resumenGeneralVerduleria archivo =
    let 
        val _ = mostrarTitulo "RESUMEN GENERAL DE LA VERDULERIA"
        val registros = leerRegistrosCSV archivo

        fun agruparPorFamilia [] = []
          | agruparPorFamilia registros =
                let
                    val familias = List.map (fn (_, _, familia, _, _) => familia) registros
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val familiasUnicas = eliminarDuplicados familias

                    fun contarFrutasEnFamilia familia =
                        let
                            val frutasEnFamilia = List.filter (fn (_, _, f, _, _) => f = familia) registros
                            val nombres = List.map (fn (_, nombre, _, _, _) => nombre) frutasEnFamilia
                            fun eliminarDuplicados [] = []
                              | eliminarDuplicados (x::xs) = 
                                    x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                            val nombresUnicos = eliminarDuplicados nombres
                        in
                            (familia, List.length nombresUnicos)
                        end
                in
                    List.map contarFrutasEnFamilia familiasUnicas
                end
        val conteoPorFamilia = agruparPorFamilia registros (*aqui guardo la cantidad de frutas por familia*)

        (* Función para encontrar frutas con mayor y menor unidades vendidas *)
        fun frutamayorymenor [] = [] 
            | frutamayorymenor registros =
                let 
                    val frutas = List.map (fn (_, nombre, _, _, _) => nombre) registros
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val frutasUnicas = eliminarDuplicados frutas

                    fun cantidadporfruta fruta = 
                        let 
                            val frutasEnFruta = List.filter (fn (_, n, _, _, _) => n = fruta) registros
                            val totalUnidades = List.foldl (fn ((_, _, _, cantidad, _), acc) => acc + cantidad) 0 frutasEnFruta
                        in
                            (fruta, totalUnidades)
                        end 

                    val frutasConCantidad = List.map cantidadporfruta frutasUnicas

                    fun frutaconmayorunidades [] = ("", 0)
                      | frutaconmayorunidades [x] = x
                      | frutaconmayorunidades ((x as (_, unidadesX)) :: resto) =
                            let
                                val mayorDelResto = frutaconmayorunidades resto
                                val (_, unidadesMayor) = mayorDelResto
                            in
                                if unidadesX >= unidadesMayor then x else mayorDelResto
                            end
                    
                    fun frutaconmenorunidades [] = ("", 999999)
                      | frutaconmenorunidades [x] = x
                      | frutaconmenorunidades ((x as (_, unidadesX)) :: resto) =
                            let
                                val menorDelResto = frutaconmenorunidades resto
                                val (_, unidadesMenor) = menorDelResto
                            in
                                if unidadesX <= unidadesMenor then x else menorDelResto
                            end

                    val frutaMayor = frutaconmayorunidades frutasConCantidad
                    val frutaMenor = frutaconmenorunidades frutasConCantidad
                in
                    [frutaMenor, frutaMayor]  (* Retorna lista con [(menor), (mayor)] *)
                end

        val frutasMayorMenor = frutamayorymenor registros (*aqui guardo la fruta con mayor y menor unidades vendidas*)

        fun familiamayormonto [] = ("", 0.0)
            | familiamayormonto registros =
                let
                    val familias = List.map (fn (_, _, familia, _, _) => familia) registros
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val familiasUnicas = eliminarDuplicados familias

                    fun montoporfamilia familia =
                        let
                            val frutasEnFamilia = List.filter (fn (_, _, f, _, _) => f = familia) registros
                            val totalMonto = List.foldl (fn ((_, _, _, cantidad, precio), acc) => acc + (Real.fromInt(cantidad) * precio)) 0.0 frutasEnFamilia
                        in
                            (familia, totalMonto)
                        end 

                    val familiasConMonto = List.map montoporfamilia familiasUnicas

                    fun familiaconmayormonto [] = ("", 0.0)
                      | familiaconmayormonto [x] = x
                      | familiaconmayormonto ((x as (_, montoX)) :: resto) =
                            let
                                val mayorDelResto = familiaconmayormonto resto
                                val (_, montoMayor) = mayorDelResto
                            in
                                if montoX >= montoMayor then x else mayorDelResto
                            end

                    val familiaMayorMonto = familiaconmayormonto familiasConMonto
                in
                    familiaMayorMonto  (* Retorna la familia con mayor monto vendido *)
                end
        val familiaMayorMonto = familiamayormonto registros (*aqui guardo la familia con mayor monto vendido*)

        fun frutaconmayormonto [] = ("", 0.0)
            | frutaconmayormonto registros =
                let 
                    val frutas = List.map (fn (_, nombre, _, _, _) => nombre) registros
                    fun eliminarDuplicados [] = []
                      | eliminarDuplicados (x::xs) = 
                            x :: eliminarDuplicados (List.filter (fn y => y <> x) xs)
                    val frutasUnicas = eliminarDuplicados frutas

                    fun montoporfruta fruta = 
                        let 
                            val frutasEnFruta = List.filter (fn (_, n, _, _, _) => n = fruta) registros
                            val totalMonto = List.foldl (fn ((_, _, _, cantidad, precio), acc) => acc + (Real.fromInt(cantidad) * precio)) 0.0 frutasEnFruta
                        in
                            (fruta, totalMonto)
                        end 

                    val frutasConMonto = List.map montoporfruta frutasUnicas

                    fun frutaconmayormonto [] = ("", 0.0)
                      | frutaconmayormonto [x] = x
                      | frutaconmayormonto ((x as (_, montoX)) :: resto) =
                            let
                                val mayorDelResto = frutaconmayormonto resto
                                val (_, montoMayor) = mayorDelResto
                            in
                                if montoX >= montoMayor then x else mayorDelResto
                            end

                    val frutaMayorMonto = frutaconmayormonto frutasConMonto
                in
                    frutaMayorMonto  (* Retorna la fruta con mayor monto vendido *)
                end
        val frutaMayorMonto = frutaconmayormonto registros (*aqui guardo la fruta con mayor monto vendido*)

        val _ = print("\n=== RESUMEN GENERAL DE LA VERDULERIA ===\n")
        
        (* 1. Cantidad de frutas por familia *)
        val _ = print("\n1. CANTIDAD DE FRUTAS POR FAMILIA:\n")
        val _ = List.app (fn (familia, count) =>
            print("   - " ^ familia ^ ": " ^ Int.toString(count) ^ " frutas diferentes\n")
        ) conteoPorFamilia

        (* 2. Fruta con mayor cantidad de unidades vendidas *)
        (* 3. Fruta con menor cantidad de unidades vendidas *)
        val _ = print("\n2. FRUTAS CON MAYOR Y MENOR UNIDADES VENDIDAS:\n")
        val _ = case frutasMayorMenor of
            [(nombreMenor, cantMenor), (nombreMayor, cantMayor)] =>
                (print("   - Fruta con MAYOR unidades vendidas: " ^ nombreMayor ^ " (" ^ Int.toString(cantMayor) ^ " unidades)\n");
                 print("   - Fruta con MENOR unidades vendidas: " ^ nombreMenor ^ " (" ^ Int.toString(cantMenor) ^ " unidades)\n"))
          | _ => print("   - No se pudieron calcular las estadísticas de mayor/menor unidades.\n")

        (* 4. Familia con mayor monto total vendido *)
        val _ = print("\n3. FAMILIA CON MAYOR MONTO TOTAL VENDIDO:\n")
        val (nombreFamilia, montoFamilia) = familiaMayorMonto
        val _ = print("   - " ^ nombreFamilia ^ ": $" ^ Real.toString(montoFamilia) ^ "\n")

        (* 5. Fruta con mayor monto total vendido *)
        val _ = print("\n4. FRUTA CON MAYOR MONTO TOTAL VENDIDO:\n")
        val (nombreFruta, montoFruta) = frutaMayorMonto
        val _ = print("   - " ^ nombreFruta ^ ": $" ^ Real.toString(montoFruta) ^ "\n")
    in
        ()
    end

fun volverMenuPrincipal () =
    print("Volviendo al menu principal...\n")

(* menuOpcionesAnalisis
   Entrada: string (archivo)
   Salida: unit
   Objetivo: Mostrar menú de opciones de análisis y manejar selección *)
fun menuOpcionesAnalisis archivo =
    let
        val _ = print("Seleccione una opcion de analisis para el archivo " ^ archivo ^ ":\n")
        val _ = print("1. Mostrar frutas populares dentro de un rango de cantidad vendida\n")
        val _ = print("2. Identificar familias con mas de 4 frutas diferentes registradas\n")
        val _ = print("3. Buscar frutas por codigo o nombre\n")
        val _ = print("4. Cantidad y lista de frutas por familia\n")
        val _ = print("5. Resumen general de la verduleria\n")
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

(* menuAnalisisRegistros
   Entrada: unit
   Salida: unit
   Objetivo: Menú principal de análisis, solicita archivo a analizar *)
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