(* Cargar módulos necesarios *)
use "auxiliares.sml";
use "creador.sml";
use "analizador.sml";

(* main
   Entrada: unit
   Salida: unit
   Objetivo: Función principal del programa, maneja menú principal *)
fun main () = 
    let 
        val _ = mostrarTitulo "GESTION DE FRUTERIA"
        val _ = print("1. Administracion de Catalogos\n")
        val _ = print("2. Analisis de Registros\n")
        val _ = print("3. Salir\n")
        val option = leerEntrada "Seleccione una opcion: "
    in
        case option of
            "1" => 
                let 
                    val _ = menuAdministracionCatalogo()
                in
                    main()  
                end
          | "2" =>
                let 
                    val _ = menuAnalisisRegistros()
                in
                    main()  
                end
          | "3" =>
                let 
                    val _ = print("Saliendo del programa...\n")
                in
                    ()  
                end
          | _ =>
                let
                    val _ = print("Opcion no valida. Por favor, intente de nuevo.\n")
                    val _ = pausar()
                in
                    main()  
                end
    end


val _ = main()

