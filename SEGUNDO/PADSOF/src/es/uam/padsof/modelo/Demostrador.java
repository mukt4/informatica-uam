package es.uam.padsof.modelo;

import es.uam.eps.sadp.grants.InvalidIDException;
import es.uam.eps.sadp.grants.InvalidRequestException;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.*;
import es.uam.padsof.modelo.fechasimulada.FechaSimulada;
import es.uam.padsof.modelo.proyecto.Ambito;
import es.uam.padsof.modelo.proyecto.Distrito;
import es.uam.padsof.modelo.proyecto.Proyecto;
import es.uam.padsof.modelo.usuario.registrado.Admin;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;

/**
 * Clase Demostrador que contiene el metodo principal para validar que los requisitos
 * se cumplan
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class Demostrador {
    public static void main(String[] args){
        Aplicacion apc;

        System.out.println("COMIENZO DEL DEMOSTRADOR");
        System.out.println();

        try {
            apc = new Aplicacion();
        } catch (IOException | ClassNotFoundException e) {
            System.out.println("Error al cargar el sistema");
            return;
        }

        /*
            SETEAMOS LA FECHA DE HOY CON EL DIA CORRECTO
         */
        FechaSimulada.restablecerHoyReal();
        System.out.println("Fecha configurada con exito");
        System.out.println();

        /*
            IMPORTANDO USUARIOS DE FICHERO
        */

        System.out.println("Importando usuarios...");
        try {
            apc.leerFicheroDatosUsuarios("usuarios.txt");
        } catch (FileNotFoundException e) {
            System.out.println("No existe ese fichero de datos");
        } catch (ErrorFichero e){
            System.out.println("Error en el formato del fichero");
            System.out.println(e);
        }
        System.out.println("Usuarios cargados con exito");
        System.out.println(apc.getUsuariosRegistrados());
        System.out.println();

        /*
            INICIO DE SESION CON EL USUARIO TOM
        */

        if(apc.login("Tom", "3atlon"))
            System.out.println("Inicio de sesion correcto");
        else
            System.out.println("Credenciales incorrectas");

        UsuarioRegistrado usuario = apc.getUsuarioActual();
        if(usuario == null){
            System.out.println("Error en el inicio de sesion");
            return;
        }
        System.out.println("El usuario " + usuario.getNombreUsuario() + " esta logueado en el sistema");
        System.out.println();

        /*
            CREACION DE COLECTIVOS DE EJEMPLO
        */

        try {
            apc.crearColectivo("Colectivo 1");
        } catch (ColectivoExcepcion colectivoExcepcion) {
            System.out.println(colectivoExcepcion);
        }

        Colectivo colectivo = apc.getColectivo("Colectivo 1");

        try {
            apc.crearColectivoHijo("Colectivo 2 hijo del colectivo 1", colectivo);
        } catch (ColectivoExcepcion | AplicacionExcepcion colectivoExcepcion) {
            System.out.println(colectivoExcepcion);
        }

        System.out.println("Colectivos creados con exito");
        System.out.println(apc.getColectivos());
        System.out.println();

        /*
            CREACION DE PROYECTO DE TIPO INFRAESTRUCTURA DE EJEMPLO
        */

        ArrayList<Distrito> distritos = new ArrayList<>();
        distritos.add(Distrito.Barajas);
        distritos.add(Distrito.Carabanchel);
        File fichero =  new File("ejemplo.jpg");
        try {
            apc.crearProyectoInfraestructura("Proyecto 1", "Este es el proyecto 1", 25, distritos, fichero, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        try {
            apc.crearProyectoInfraestructura("Proyecto 2", "Este es el proyecto 2 creado por el colectivo 1", 50, colectivo, distritos, fichero, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }
        System.out.println("Proyectos de tipo infraestructura creados con exito");
        System.out.println("Listado de proyectos: ");
        System.out.println(apc.getProyectos());
        System.out.println();

        /*
            CREACION DE PROYECTO DE TIPO SOCIAL DE EJEMPLO
         */

        Ambito ambito = Ambito.INTERNACIONAL;
        try {
            apc.crearProyectoSocial("Proyecto 3", "Este es el proyecto 3", 25, "Este es el grupo social del proyecto 3", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        try {
            apc.crearProyectoSocial("Proyecto 4", "Este es el proyecto 4 creado por el colectivo 1", 50, colectivo, "Este es el grupo social del proyecto 4", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }
        System.out.println("Proyectos de tipo social creados con exito");
        System.out.println("Listado de proyectos: ");
        ArrayList<Proyecto> proyectos = apc.getProyectos();
        System.out.println(proyectos);
        System.out.println();

        /*
            CREACION DE PROYECTO NUEVO CON FECHA AVANZADA PARA QUE NO CADUQUE
         */

        FechaSimulada.avanzar(25);
        LocalDate fecha = FechaSimulada.getHoy();
        FechaSimulada.restablecerHoyReal();

        try {
            apc.crearProyectoSocial("Proyecto 5", "Este es el proyecto 5", 25, "Este es el grupo social del proyecto 5", ambito, fecha);
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        System.out.println("Creacion de proyecto con fecha avanzada para que no caduque");
        System.out.println();

        /*
            VOTAR PROYECTO
        */
        Proyecto proyecto = proyectos.get(0);

        try {
            apc.votarProyecto(proyecto);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        System.out.println("Proyecto 1 votado con exito");
        System.out.println();

        /*
            VOTAR PROYECTO COMO COLECTIVO
        */

        try {
            apc.votarProyecto(colectivo, proyecto);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        System.out.println("Proyecto 1 votado con exito con colectivo 1");
        System.out.println("Votos del proyecto 1: " + proyecto.getVotos());
        System.out.println();

        /*
            SEGUIR PROYECTO COMO COLECTIVO
        */

        proyecto.seguirProyecto(colectivo);
        System.out.println("Proyecto seguido como colectivo con exito");
        System.out.println(proyecto);
        System.out.println();

        /*
            OBTENER PROYECTOS A LOS QUE SIGUES COMO COLECTIVO
        */

        proyectos = apc.getProyectosSeguidos(colectivo);
        System.out.println("Obtener proyectos a los que sigues como colectivo con exito");
        System.out.println(proyectos);
        System.out.println();


        /*
            ABANDONAR PROYECTO COMO COLECTIVO
        */

        proyecto.abandonarProyecto(colectivo);
        System.out.println("Proyecto abandonado como colectivo con exito");
        System.out.println(proyecto);
        System.out.println();


        /*
            LOG OUT DEL SISTEMA
        */

        apc.logout();
        System.out.println("Log out realizado con exito");
        System.out.println();

        /*
            INICIO DE SESION CON OTRO USUARIO
        */

        if(apc.login("Sergio", "asdasd"))
            System.out.println("Inicio de sesion correcto");
        else{
            System.out.println("Credenciales incorrectas");
        }
        usuario = apc.getUsuarioActual();
        if(usuario == null){
            System.out.println("Error en el inicio de sesion");
            return;
        }
        System.out.println("El usuario " + usuario.getNombreUsuario() + " esta logueado en el sistema");
        System.out.println();

        /*
            VOTAR PROYECTO PARA PODER VER SI SUPERA UMBRAL DE VOTOS
         */
        try {
            apc.votarProyecto(proyecto);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        System.out.println("Proyecto votado para despues comprobar si ha superado umbral de votos");
        System.out.println();


        /*
            SEGUIR COLECTIVO PARA RECIBIR NOTIFICACIONES USUARIO
        */

        colectivo.seguirColectivo(usuario);
        System.out.println("Colectivo seguido con exito");
        System.out.println(colectivo);
        System.out.println();

        /*
            OBTENER COLECTIVOS A LOS QUE SIGUES
        */
        ArrayList<Colectivo> colectivos = apc.getColectivosSeguidos();
        System.out.println("Obtener colectivos a los que sigues con exito");
        System.out.println(colectivo);
        System.out.println();

        /*
            DEJAR DE SEGUIR COLECTIVO
        */

        colectivo.abandonarColectivo(usuario);
        System.out.println("Colectivo abandonado con exito");
        System.out.println(colectivo);
        System.out.println();

        /*
            SEGUIR PROYECTO PARA RECIBIR NOTIFICACIONES
        */

        proyecto.seguirProyecto(usuario);
        System.out.println("Proyecto seguido con exito");
        System.out.println(proyecto);
        System.out.println();

        /*
            OBTENER PROYECTOS A LOS QUE SIGUES
        */

        proyectos = apc.getProyectosSeguidos();
        System.out.println("Obtener proyectos a los que sigues con exito");
        System.out.println(proyectos);
        System.out.println();

        /*
            DEJAR DE SEGUIR PROYECTO
        */

        proyecto.abandonarProyecto(usuario);
        System.out.println("Proyecto abandonado con exito");
        System.out.println(proyecto);
        System.out.println();

        /*
            LOG OUT DEL SISTEMA
        */
        apc.logout();
        System.out.println("Log out realizado con exito");
        System.out.println();


        /*
            INICIO DE SESION COMO ADMIN
         */

        if(apc.login("admin", "admin"))
            System.out.println("Inicio de sesion correcto");
        else
            System.out.println("Credenciales incorrectas");

        Admin admin = apc.getAdmin();

        if(admin != null) {
            System.out.println("Sesion iniciada como administrador");
            System.out.println();
        }
        /*
            CAMBIAR UMBRAL DE VOTOS
        */

        try {
            apc.setNumero_votos(1);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }

        System.out.println("Umbral de votos cambiado con exito");
        System.out.println("Umbral de votos actual: " + apc.getNumero_votos());
        System.out.println();

        /*
            COMPROBAR SI PROYECTO HA SUPERADO UMBRAL DE VOTOS
         */
        apc.comprobarVotos();
        proyectos = apc.getProyectosPendientes();
        System.out.println("Obtener proyectos que han superado umbral de votos con exito");
        System.out.println(proyectos);
        System.out.println();


        /*
            BLOQUEAR USUARIO
         */

        try {
            apc.bloquearUsuario(usuario);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        System.out.println("Usuario baneado con exito");
        System.out.println(usuario);
        System.out.println();

        /*
            DESBLOQUEAR USUARIO
        */

        try {
            apc.desbloquearUsuario(usuario);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        System.out.println("Usuario desbloqueado con exito");
        System.out.println(usuario);
        System.out.println();

        /*
            RECHAZAR PROYECTO
        */

        try {
            apc.rechazarProyecto(proyecto, "Proyecto 1 rechazado");
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        System.out.println("Proyecto aceptado con exito");
        System.out.println(proyecto);
        System.out.println();

        /*
            OBTENER PROYECTOS RECHAZADOS
        */

        proyectos = apc.getProyectosRechazados();
        System.out.println("Obtener proyectos rechazados con exito");
        System.out.println(proyectos);
        System.out.println();

        /*
            ACEPTAR PROYECTO
        */

        try {
            apc.aceptarProyecto(proyecto);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        System.out.println("Proyecto aceptado con exito");
        System.out.println(proyecto);
        System.out.println();

        /*
            OBTENER PROYECTOS ACEPTADOS
         */

        proyectos = apc.getProyectosAceptados();
        System.out.println("Obtener proyectos aceptados con exito");
        System.out.println(proyectos);
        System.out.println();

        /*
            AVANZAR DIAS PARA OBTENER PROYECTOS CADUCADOS
         */

        System.out.println("Avanzamos 35 dias para ver proyectos caducados");
        FechaSimulada.avanzar(35);

        /*
            OBTENER PROYECTOS CADUCADOS
        */
        apc.comprobarCaducados();
        proyectos = apc.getProyectosCaducados();
        System.out.println("Obtener proyectos caducados con exito");
        System.out.println(proyectos);
        System.out.println();

        /*
            OBTENER PROYECTOS DISPONIBLES
         */
        proyectos = apc.getProyectosDisponibles();
        System.out.println("Obtener proyectos disponibles con exito");
        System.out.println(proyectos);
        System.out.println();

        /*
            RECHAZAR PROYECTO
         */
        try {
            apc.rechazarProyecto(proyecto, "Este proyecto ha sido rechazado");
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }

        /*
            CERRAR SESION COMO ADMIN
        */

        apc.logout();
        System.out.println("Log out realizado con exito");
        System.out.println();


        /*
            INICIO DE SESION CON USUARIO TOM PARA COMPROBAR NOTIFICACIONES
         */

        if(apc.login("Tom", "3atlon"))
            System.out.println("Inicio de sesion correcto");
        else
            System.out.println("Credenciales incorrectas");

        usuario = apc.getUsuarioActual();
        if(usuario == null){
            System.out.println("Error en el inicio de sesion");
            return;
        }
        System.out.println("El usuario " + usuario.getNombreUsuario() + " esta logueado en el sistema");
        System.out.println();

        /*
            CREACION DE UN NUEVO PROYECTO PARA SIMULAR NOTIFICACIONES
         */

        try {
            apc.crearProyectoSocial("Proyecto 3", "Este es el proyecto 3", 25, "Este es el grupo social del proyecto 3", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }
        proyectos = apc.getProyectos();
        proyecto = proyectos.get(proyectos.size() - 1);

        /*
            LIMPIAMOS NOTIFICACIONES
         */
        apc.limpiarNotificacionesUsuario();

        /*
            RECHAZAMOS EL PROYECTO
         */
        proyecto.rechazado("Este proyecto ha sido rechazado");

        /*
            IMPRIMIMOS NOTIFICACIONES
         */
        ArrayList<String> notificaciones = apc.getNotificaciones();
        System.out.println("Notificaciones obtenidas con exito");
        System.out.println(notificaciones);
        System.out.println();

        /*
            CONSULTAR AFINIDAD ENTRE COLECTIVOS CREAREMOS VARIOS PROYECTOS DE TEST ASI COMO DOS COLECTIVOS NUEVOS DE TEST
         */

        // CREACION DE PROYECTOS DE TEST

        try {
            apc.crearProyectoSocial("Proyecto afinidad 1", "Este es el proyecto para comprobar afinidad", 25, "Este es el grupo social del proyecto afinidad", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        try {
            apc.crearProyectoSocial("Proyecto afinidad 2", "Este es el proyecto para comprobar afinidad", 25, "Este es el grupo social del proyecto afinidad", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        try {
            apc.crearProyectoSocial("Proyecto afinidad 3", "Este es el proyecto para comprobar afinidad", 25, "Este es el grupo social del proyecto afinidad", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        try {
            apc.crearProyectoSocial("Proyecto afinidad 4", "Este es el proyecto para comprobar afinidad", 25, "Este es el grupo social del proyecto afinidad", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        try {
            apc.crearProyectoSocial("Proyecto afinidad 5", "Este es el proyecto para comprobar afinidad", 25, "Este es el grupo social del proyecto afinidad", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion proyectoExcepcion) {
            System.out.println(proyectoExcepcion);
        }

        // CREACION DE COLECTIVOS DE TEST

        try {
            apc.crearColectivo("Colectivo A");
        } catch (ColectivoExcepcion colectivoExcepcion) {
            System.out.println(colectivoExcepcion);
        }

        Colectivo colectivoA = apc.getColectivo("Colectivo A");

        try {
            apc.crearColectivo("Colectivo B");
        } catch (ColectivoExcepcion colectivoExcepcion) {
            System.out.println(colectivoExcepcion);
        }

        Colectivo colectivoB = apc.getColectivo("Colectivo B");

        /*
            SEGUIMOS A LOS DIFERENTES PROYECTOS PARA CONSULTAR AFINIDAD
         */

        proyectos = apc.getProyectos();
        proyectos.get(proyectos.size() - 1).seguirProyecto(colectivoA);
        proyectos.get(proyectos.size() - 1).seguirProyecto(colectivoB);
        proyectos.get(proyectos.size() - 2).seguirProyecto(colectivoA);
        proyectos.get(proyectos.size() - 2).seguirProyecto(colectivoB);
        proyectos.get(proyectos.size() - 3).seguirProyecto(colectivoA);
        proyectos.get(proyectos.size() - 4).seguirProyecto(colectivoB);
        proyectos.get(proyectos.size() - 5).seguirProyecto(colectivoA);

        /*
            CONSULTAR AFINIDAD
         */
        try {
            double afinidad = apc.consultarAfinidad(colectivoA, colectivoB);
            System.out.println("Consultar afinidada con exito: ");
            System.out.println("Afinidad entre ambos grupos: " + afinidad);
            System.out.println();
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }

        /*
            SOLICITAR FINANCIACION DE UN PROYECTO
         */
        try {
            proyecto = apc.getProyecto_i(0);
            apc.setVotos(1);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        try {
            apc.solicitarFinanciacion(proyecto);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        } catch (IOException e) {
            System.out.println("Problemas al conectarse con el servidor de validacion");
        } catch (InvalidRequestException e) {
            System.out.println("Solicitud al servidor realizada de manera incorrecta");
        }
        System.out.println("Financiacion solicitada con exito");
        System.out.println(proyecto);
        System.out.println();

        System.out.println("Avanzamos 7 dias para comprobar si el proyecto ha sido financiado");
        FechaSimulada.avanzar(7);
        apc.comprobarProyectosEspera();
        System.out.println("Respuesta a financiacion realizada con exito");
        System.out.println(proyecto);
        System.out.println();


        /*
            CERRAR SESION
        */

        apc.logout();
        System.out.println("Log out realizado con exito");
        System.out.println();

        System.out.println("DEMOSTRADOR FINALIZADO");
        System.out.println();
    }
}
