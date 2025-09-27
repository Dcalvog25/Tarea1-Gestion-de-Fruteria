
(* leerEntrada
   Entrada: string (prompt a mostrar)
   Salida: string (entrada del usuario sin \n)
   Objetivo: Capturar entrada del usuario desde consola *)
fun leerEntrada prompt =
    let
        val _ = print(prompt)
        val entrada = TextIO.inputLine TextIO.stdIn
    in
        case entrada of
            SOME s => String.substring(s, 0, String.size(s) - 1) (* Elimina \n *)
          | NONE => ""
    end

(* esEntero
   Entrada: string
   Salida: bool
   Objetivo: Validar si un string representa un número entero válido *)
fun esEntero s = 
    case Int.fromString s of
        SOME _ => true
      | NONE => false

(* esReal
   Entrada: string
   Salida: bool
   Objetivo: Validar si un string representa un número real válido *)
fun esReal s = 
    case Real.fromString s of
        SOME _ => true
      | NONE => false

(* leerEntero
   Entrada: string (prompt)
   Salida: int
   Objetivo: Leer y validar entrada de número entero con reintentos *)
fun leerEntero prompt =
    let
        fun intentarLeer () =
            let
                val entrada = leerEntrada prompt
            in
                if esEntero entrada then
                    case Int.fromString entrada of
                        SOME n => n
                      | NONE => (print("Error al convertir número.\n"); intentarLeer())
                else
                    (print("Debe ingresar un número entero válido. Intente de nuevo.\n"); intentarLeer())
            end
    in
        intentarLeer()
    end

(* leerReal
   Entrada: string (prompt)
   Salida: real
   Objetivo: Leer y validar entrada de número real con reintentos *)
fun leerReal prompt =
    let
        fun intentarLeer () =
            let
                val entrada = leerEntrada prompt
            in
                if esReal entrada then
                    case Real.fromString entrada of
                        SOME r => r
                      | NONE => (print("Error al convertir número.\n"); intentarLeer())
                else
                    (print("Debe ingresar un número real válido. Intente de nuevo.\n"); intentarLeer())
            end
    in
        intentarLeer()
    end



(* unirStrings
   Entrada: string (separador), string list
   Salida: string
   Objetivo: Concatenar lista de strings con separador específico *)
fun unirStrings separador lista =
    case lista of
        [] => ""
      | [x] => x
      | x :: xs => x ^ separador ^ unirStrings separador xs


(* archivoExiste
   Entrada: string (nombre del archivo)
   Salida: bool
   Objetivo: Verificar existencia de archivo sin generar excepción *)
fun archivoExiste nombreArchivo =
    (TextIO.openIn nombreArchivo; TextIO.closeIn (TextIO.openIn nombreArchivo); true)
    handle IO.Io _ => false

(* crearArchivoSiNoExiste
   Entrada: string (archivo), string (contenido inicial)
   Salida: unit
   Objetivo: Crear archivo con contenido solo si no existe previamente *)
fun crearArchivoSiNoExiste nombreArchivo contenidoInicial =
    if not (archivoExiste nombreArchivo) then
        let 
            val archivo = TextIO.openOut nombreArchivo
            val _ = TextIO.output(archivo, contenidoInicial)
            val _ = TextIO.closeOut archivo
        in
            print("Archivo " ^ nombreArchivo ^ " creado.\n")
        end
    else
        ()



(* pausar
   Entrada: unit
   Salida: unit
   Objetivo: Pausar ejecución hasta que usuario presione Enter *)
fun pausar () =
    let
        val _ = print("Presione Enter para continuar...\n")
        val _ = TextIO.inputLine TextIO.stdIn
    in
        ()
    end

(* mostrarTitulo
   Entrada: string (título)
   Salida: unit
   Objetivo: Mostrar título centrado con decoración *)
fun mostrarTitulo titulo =
    let
        val linea = "========================================"
        val espacios = String.implode (List.tabulate(3, fn _ => #" "))
    in
        print(linea ^ "\n" ^ espacios ^ titulo ^ "\n" ^ linea ^ "\n")
    end

(* confirmarAccion
   Entrada: string (mensaje)
   Salida: bool
   Objetivo: Solicitar confirmación s/n del usuario *)
fun confirmarAccion mensaje =
    let
        val _ = print(mensaje ^ " (s/n): ")
        val respuesta = TextIO.inputLine TextIO.stdIn
    in
        case respuesta of
            SOME "s\n" => true
          | SOME "S\n" => true
          | _ => false
    end


(* dividirCSV
   Entrada: string (línea CSV)
   Salida: string list (campos separados)
   Objetivo: Parsear línea CSV separando por comas *)
fun dividirCSV linea =
    let
        fun dividir [] acc current = rev (current :: acc)
          | dividir (#"," :: resto) acc current = dividir resto (current :: acc) ""
          | dividir (c :: resto) acc current = dividir resto acc (current ^ String.str(c))
    in
        dividir (String.explode linea) [] ""
    end

(* lineaARegistro
   Entrada: string (línea CSV)
   Salida: (string * string * string * int * real) option
   Objetivo: Convertir línea CSV a tupla de registro de fruta *)
fun lineaARegistro linea =
    let
        val campos = dividirCSV linea
    in
        case campos of
            [codigo, nombre, familia, cantidadStr, precioStr] =>
                (case (Int.fromString cantidadStr, Real.fromString precioStr) of
                     (SOME cantidad, SOME precio) => 
                         SOME (codigo, nombre, familia, cantidad, precio)
                   | _ => NONE)
          | _ => NONE
    end

(* leerRegistrosCSV
   Entrada: string (nombre del archivo)
   Salida: (string * string * string * int * real) list
   Objetivo: Leer todos los registros válidos de un archivo CSV *)
fun leerRegistrosCSV nombreArchivo =
    let
        val archivo = TextIO.openIn nombreArchivo
        
        fun leerLineas acc =
            case TextIO.inputLine archivo of
                SOME linea => 
                    if String.isPrefix "codigo," linea then 
                        leerLineas acc
                    else
                        let
                            val lineaLimpia = String.substring(linea, 0, String.size(linea) - 1)
                        in
                            case lineaARegistro lineaLimpia of
                                SOME registro => leerLineas (registro :: acc)
                              | NONE => leerLineas acc
                        end
              | NONE => rev acc

        val registros = leerLineas []
        val _ = TextIO.closeIn archivo
    in
        registros
    end



(* aMinusculas
   Entrada: string
   Salida: string
   Objetivo: Convertir string a minúsculas para búsquedas insensibles *)
fun aMinusculas s =
    String.implode (List.map (fn c => 
        if #"A" <= c andalso c <= #"Z" then 
            Char.chr(Char.ord(c) + 32) 
        else c) (String.explode s))

(* contieneCadena
   Entrada: string (búsqueda), string (texto)
   Salida: bool
   Objetivo: Verificar si texto contiene subcadena (insensible a mayúsculas) *)
fun contieneCadena busqueda texto =
    let
        val busquedaMin = aMinusculas busqueda
        val textoMin = aMinusculas texto
    in
        String.isSubstring busquedaMin textoMin
    end

(* mostrarRegistroFruta
   Entrada: (string * string * string * int * real)
   Salida: unit
   Objetivo: Mostrar registro de fruta formateado con totales *)
fun mostrarRegistroFruta (codigo, nombre, familia, cantidad, precio) =
    let
        val total = Real.fromInt(cantidad) * precio
    in
        print("  Codigo: " ^ codigo ^ "\n");
        print("  Nombre: " ^ nombre ^ "\n");
        print("  Familia: " ^ familia ^ "\n");
        print("  Cantidad vendida: " ^ Int.toString(cantidad) ^ " unidades\n");
        print("  Precio unitario: $" ^ Real.toString(precio) ^ "\n");
        print("  Total de venta: $" ^ Real.toString(total) ^ "\n");
        print("  " ^ String.implode (List.tabulate(40, fn _ => #"-")) ^ "\n")
    end



