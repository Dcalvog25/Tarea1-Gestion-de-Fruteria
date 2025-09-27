
(* Función para leer una línea y eliminar el salto de línea *)
fun leerEntrada prompt =
    let
        val _ = print(prompt)
        val entrada = TextIO.inputLine TextIO.stdIn
    in
        case entrada of
            SOME s => String.substring(s, 0, String.size(s) - 1) (* Elimina \n *)
          | NONE => ""
    end

(* Función para validar que un string sea un entero *)
fun esEntero s = 
    case Int.fromString s of
        SOME _ => true
      | NONE => false

(* Función para validar que un string sea un real *)
fun esReal s = 
    case Real.fromString s of
        SOME _ => true
      | NONE => false

(* Función para leer un entero con validación *)
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

(* Función para leer un real con validación *)
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



(* Función para unir strings con un separador *)
fun unirStrings separador lista =
    case lista of
        [] => ""
      | [x] => x
      | x :: xs => x ^ separador ^ unirStrings separador xs


(* Función para verificar si un archivo existe *)
fun archivoExiste nombreArchivo =
    (TextIO.openIn nombreArchivo; TextIO.closeIn (TextIO.openIn nombreArchivo); true)
    handle IO.Io _ => false

(* Función para crear archivo con contenido inicial si no existe *)
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



(* Función para pausar y esperar Enter *)
fun pausar () =
    let
        val _ = print("Presione Enter para continuar...\n")
        val _ = TextIO.inputLine TextIO.stdIn
    in
        ()
    end

(* Función para mostrar un título centrado *)
fun mostrarTitulo titulo =
    let
        val linea = "========================================"
        val espacios = String.implode (List.tabulate(3, fn _ => #" "))
    in
        print(linea ^ "\n" ^ espacios ^ titulo ^ "\n" ^ linea ^ "\n")
    end

(* Función para mostrar mensaje con confirmación s/n *)
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


(* Función para dividir una línea CSV *)
fun dividirCSV linea =
    let
        fun dividir [] acc current = rev (current :: acc)
          | dividir (#"," :: resto) acc current = dividir resto (current :: acc) ""
          | dividir (c :: resto) acc current = dividir resto acc (current ^ String.str(c))
    in
        dividir (String.explode linea) [] ""
    end

(* Función para convertir línea CSV a tupla de registro de fruta *)
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

(* Función para leer todos los registros de un archivo CSV *)
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



(* Función para convertir string a minúsculas*)
fun aMinusculas s =
    String.implode (List.map (fn c => 
        if #"A" <= c andalso c <= #"Z" then 
            Char.chr(Char.ord(c) + 32) 
        else c) (String.explode s))

(* Función para verificar si un string contiene otro *)
fun contieneCadena busqueda texto =
    let
        val busquedaMin = aMinusculas busqueda
        val textoMin = aMinusculas texto
    in
        String.isSubstring busquedaMin textoMin
    end

(* Función para mostrar un registro de fruta *)
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



