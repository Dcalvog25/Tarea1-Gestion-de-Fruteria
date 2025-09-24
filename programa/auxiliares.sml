
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



