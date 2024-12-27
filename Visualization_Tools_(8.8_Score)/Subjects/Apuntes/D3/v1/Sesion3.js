/* Ejecución Dinámica del fichero (Archivo JavaScript) */
console.log("Hola q tal")

d3.json("http://output.jsbin.com/lixujex/1.js").then(
    function(datos) {
        console.log("Ya hemos cargado los datos")
        
        // Creamos una variable global 
        window.datosglobal = datos

        // Crear listas con D3
        var elementoUl = d3.select("body").append("ul")

        // Introducir todas las etiquetas li necesarias
        elementoUl
            .selectAll ("li") // seleccion de tantas li como haga falta
            .data (datos) // J0IN
            .enter ()
            .append ("li")
            .text (function (d) {return d.partido})
    }
)