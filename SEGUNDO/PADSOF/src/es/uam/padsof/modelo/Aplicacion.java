package es.uam.padsof.modelo;

import es.uam.eps.sadp.grants.CCGG;
import es.uam.eps.sadp.grants.InvalidIDException;
import es.uam.eps.sadp.grants.InvalidRequestException;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.*;
import es.uam.padsof.modelo.fechasimulada.FechaSimulada;
import es.uam.padsof.modelo.proyecto.*;
import es.uam.padsof.modelo.usuario.registrado.Admin;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.io.*;
import java.time.LocalDate;

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.UUID;



public class Aplicacion implements Serializable{
    /**
     * Serial Version para serializar la aplicacion
     */
    private static final long serialVersionUID = 1L;

    /**
     * ArrayList que contiene los colectivos de la aplicacion
     */
    private ArrayList<Colectivo> colectivos;

    /**
     * ArrayList que contiene los proyectos de la aplicacion
     */
    private ArrayList<Proyecto> proyectos;

    /**
     * ArrayList que contiene los usuarios registrados de la aplicacion
     */
    private ArrayList<UsuarioRegistrado> usuariosRegistrados;

    /**
     * Usuario que se encuentra utilizando la aplicacion
     */
    private UsuarioRegistrado usuarioActual;

    /**
     * Administrador que se encuentra utilizando la aplicacion
     */
    private Admin admin;

    /**
     * Numero de votos solicitados en la aplicacion para poder solicitar financiacion
     */
    private int numero_votos;

    /**
     * Constructor de la clase aplicacion
     * @throws IOException Esta excepcion se lanzara si se produce un error deserealizando la aplicacion
     * @throws ClassNotFoundException Esta excepcion se lanzara si se produce un error deserealizando la aplicacion
     */
    public Aplicacion() throws IOException, ClassNotFoundException {
        colectivos = new ArrayList<>();
        usuariosRegistrados = new ArrayList<>();
        proyectos = new ArrayList<>();
        usuarioActual = null;
        admin = null;
        numero_votos = 5;
        Aplicacion instanciaAplicacion;

        File fSysSaveFile = new File("apc.ser");
        if(fSysSaveFile.exists()){
            // Deserealizar sistema
            FileInputStream sysFis = new FileInputStream("apc.ser");
            ObjectInputStream sysOis = new ObjectInputStream(sysFis);
            instanciaAplicacion = (Aplicacion) sysOis.readObject();
            sysOis.close();
            sysFis.close();
            this.colectivos = instanciaAplicacion.colectivos;
            this.proyectos = instanciaAplicacion.proyectos;
            this.usuariosRegistrados = instanciaAplicacion.usuariosRegistrados;
            this.numero_votos = instanciaAplicacion.numero_votos;
            comprobaciones();
        }else{
            this.colectivos = new ArrayList<>();
            this.proyectos = new ArrayList<>();
            this.usuariosRegistrados = new ArrayList<>();
        }
    }

    /**
     * Metodo que serializa el objeto Aplicacion y lo guarda en un fichero apc.ser
     * @throws IOException Esta excepcion se lanzara si ha habido un error guardando en el fichero
     */
    public void guardarDatos() throws IOException {
        FileOutputStream sysFos = new FileOutputStream("apc.ser");
        ObjectOutputStream sysOos = new ObjectOutputStream(sysFos);
        sysOos.writeObject(this);
        sysOos.close();
        sysFos.close();
    }

    /**
     * Metodo que registra a un usuario en la aplicacion
     * @param usuario String que contiene el nombre del usuario
     * @param dni String que contiene el dni del usuario
     * @param contrasena String que contiene la contrasena del usuario
     * @throws UsuarioExcepcion Esta excepcion se lanzara si se produce un error en el registro
     */
    public void registrar(String usuario, String dni, String contrasena) throws UsuarioExcepcion {
        // Comprobamos que el dni sea correcto
        UsuarioRegistrado usuarioNuevo;

        for(UsuarioRegistrado u : usuariosRegistrados){
            if(u.getDni().equals(dni) || u.getNombreUsuario().equals(usuario) || u.getNombreUsuario().equals("admin")) {
                throw new UsuarioExcepcion("Ya existe un usuario con ese nombre o DNI");
            }
        }

        usuarioNuevo = new UsuarioRegistrado(dni, contrasena, usuario);
        usuariosRegistrados.add(usuarioNuevo);
        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }

    /**
     * Metodo que loguea un usuario en la aplicacion
     * @param usuario String que contiene el nombre del usuario
     * @param contrasenia String que contiene la contrasena del usuario
     * @return boolean: true si el login es correcto o false si no lo es
     */
    public boolean login(String usuario, String contrasenia){
        if(usuario.equals("admin") && contrasenia.equals("admin")) {
            admin = new Admin();
            return true;
        }
        else {
            for(UsuarioRegistrado u : usuariosRegistrados){
                if(u.getNombreUsuario().equals(usuario) && u.getContrasena().equals(contrasenia)){
                    usuarioActual = u;
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * Metodo que hace logout de un usuario de la aplicacion
     */
    public void logout(){
        if(usuarioActual != null || admin != null) {
            usuarioActual = null;
            admin = null;
            try {
                guardarDatos();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Metodo que anade un colectivo en la aplicacion
     * @param nombre String que contiene el nombre del colectivo
     * @throws ColectivoExcepcion Esta excepcion se lanzara si no se cumple alguno de los requisitos para crear un colectivo
     */
    public void crearColectivo(String nombre) throws ColectivoExcepcion {
        if(usuarioActual.isBloqueado()) {
            throw new ColectivoExcepcion("Un usuario bloqueado no puede crear un colectivo");
        }
        for(Colectivo c : colectivos){
            if(c.comprobarNombre(nombre)){
                throw new ColectivoExcepcion("Ya existe un colectivo con ese nombre");
            }
        }
        colectivos.add(new Colectivo(nombre, usuarioActual));

        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }

    /**
     * Metodo que crea un colectivo hijo dentro de otro colectivo
     * @param nombre String que contiene el nombre del colectivo hijo
     * @param colectivoPadre Colectivo que hace de padre
     * @throws ColectivoExcepcion Esta excepcion se lanzara si no se cumple alguno de los requisitos para crear un colectivo
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error al crear el colectivo
     */
    public void crearColectivoHijo(String nombre, Colectivo colectivoPadre) throws AplicacionExcepcion, ColectivoExcepcion {
        int flag = 0;

        if(usuarioActual.isBloqueado()) {
            throw new ColectivoExcepcion("Un usuario bloqueado no puede crear un colectivo");
        }
        for(Colectivo c : colectivos){
            if(c.comprobarNombre(nombre)){
                throw new ColectivoExcepcion("Ya existe un colectivo con ese nombre");
            }
            if(c.comprobarNombre(colectivoPadre.getNombre()))
                flag++;
        }
        if(flag == 0)
            throw new AplicacionExcepcion("No existe el proyecto padre en la aplicacion");

        colectivoPadre.crearColectivoHijo(nombre);

        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }

    /**
     * Este metodo anadira un proyecto de tipo social en la aplicacion con un usuario como representante
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param ambito Ambito que contiene el ambito del proyecto
     * @param grupoSocial String que contiene el grupo social al que va dirigido el proyecto
     * @throws ProyectoExcepcion Esta excepcion se lanzara si no se cumple alguno de los requisitos para crear un proyecto
     */
    public void crearProyectoSocial(String titulo, String descripcion, double importe, String grupoSocial, Ambito ambito, LocalDate fecha) throws ProyectoExcepcion {
        // Comprobamos primero si los tamanos tanto de descripcion como de titulo son correctos, ademas en este caso al ser social
        // tambien comprobamos si la longitud del grupo social es tambien correcta
        if(titulo.length() > 25){
            throw new ProyectoExcepcion("Titulo de proyecto con mas de 25 caracteres");
        }
        if(descripcion.length() > 500){
            throw new ProyectoExcepcion("Descripcion del proyecto con mas de 500 caracteres");
        }
        if(grupoSocial.length() > 50){
            throw new ProyectoExcepcion("Grupo social con mas de 50 caracteres");
        }
        if(importe < 0){
            throw new ProyectoExcepcion("Dinero solicitado erroneo");
        }
        if(usuarioActual.isBloqueado()){
            throw new ProyectoExcepcion("Un usuario bloqueado no puede crear proyectos");
        }
        proyectos.add(new ProyectoSocial(titulo, descripcion, importe, usuarioActual, fecha, grupoSocial, ambito));
        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }

    /**
     * Este metodo anadira un proyecto de tipo infraestructura en la aplicacion con un usuario como representante
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param distritos ArrayList que contiene los distritos del proyecto
     * @param esquemaGrafico File que contiene esquema grafico del proyecto
     * @throws ProyectoExcepcion Esta excepcion se lanzara si no se cumple alguno de los requisitos para crear un proyecto
     */
    public void crearProyectoInfraestructura(String titulo, String descripcion, double importe, ArrayList<Distrito> distritos, File esquemaGrafico, LocalDate fecha) throws ProyectoExcepcion {
        // Comprobamos primero si el titulo y la descripcion del proyecto tienen el tamano corecto
        if(titulo.length() > 25){
            throw new ProyectoExcepcion("Titulo de proyecto con mas de 25 caracteres");
        }
        if(descripcion.length() > 500){
            throw new ProyectoExcepcion("Descripcion del proyecto con mas de 500 caracteres");
        }
        if(importe < 0){
            throw new ProyectoExcepcion("Dinero solicitado erroneo");
        }
        if(usuarioActual.isBloqueado()){
            throw new ProyectoExcepcion("Un usuario bloqueado no puede crear proyectos");
        }
        proyectos.add(new Infraestructura(titulo, descripcion, importe, usuarioActual, fecha, distritos, esquemaGrafico));
        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }


    /**
     * Este metodo anadira un proyecto de tipo social en la aplicacion con un colectivo como representante
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param colectivoCreador Colectivo creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param ambito Ambito que contiene el ambito del proyecto
     * @param grupoSocial String que contiene el grupo social al que va dirigido el proyecto
     * @throws ProyectoExcepcion Esta excepcion se lanzara si no se cumple alguno de los requisitos para crear un proyecto
     */
    public void crearProyectoSocial(String titulo, String descripcion, double importe, Colectivo colectivoCreador, String grupoSocial, Ambito ambito, LocalDate fecha) throws ProyectoExcepcion {
        // Comprobamos primero si los tamanos tanto de descripcion como de titulo son correctos, ademas en este caso al ser social
        // tambien comprobamos si la longitud del grupo social es tambien correcta
        if(titulo.length() > 25){
            throw new ProyectoExcepcion("Titulo de proyecto con mas de 25 caracteres");
        }
        if(descripcion.length() > 500){
            throw new ProyectoExcepcion("Descripcion del proyecto con mas de 500 caracteres");
        }
        if(grupoSocial.length() > 50){
            throw new ProyectoExcepcion("Grupo social con mas de 50 caracteres");
        }
        if(importe < 0){
            throw new ProyectoExcepcion("Dinero solicitado erroneo");
        }
        if(usuarioActual.isBloqueado()){
            throw new ProyectoExcepcion("Un usuario bloqueado no puede crear proyectos");
        }
        if(colectivoCreador.comprobarCreador(usuarioActual.getNombreUsuario())) {
            proyectos.add(new ProyectoSocial(titulo, descripcion, importe, colectivoCreador, fecha, grupoSocial, ambito));
            try {
                this.guardarDatos();
            } catch (IOException e) {
                System.out.println("Error al guardar el estado del sistema");
            }
        }
        else{
            throw new ProyectoExcepcion("Usuario creador no se corresponde con el representante del grupo");
        }
    }

    /**
     * Este metodo anadira un proyecto de tipo infraestructura en la aplicacion con un colectivo como representante
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param colectivoCreador Colectivo creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param distritos ArrayList que contiene los distritos del proyecto
     * @param esquemaGrafico File que contiene esquema grafico del proyecto
     * @throws ProyectoExcepcion Esta excepcion se lanzara si no se cumple alguno de los requisitos para crear un proyecto
     */
    public void crearProyectoInfraestructura(String titulo, String descripcion, double importe, Colectivo colectivoCreador, ArrayList<Distrito> distritos, File esquemaGrafico, LocalDate fecha) throws ProyectoExcepcion {
        // Comprobamos primero si el titulo y la descripcion del proyecto tienen el tamano corecto
        if(titulo.length() > 25){
            throw new ProyectoExcepcion("Titulo de proyecto con mas de 25 caracteres");
        }
        if(descripcion.length() > 500){
            throw new ProyectoExcepcion("Descripcion del proyecto con mas de 500 caracteres");
        }
        if(importe < 0){
            throw new ProyectoExcepcion("Dinero solicitado erroneo");
        }
        if(usuarioActual.isBloqueado()){
            throw new ProyectoExcepcion("Un usuario bloqueado no puede crear proyectos");
        }
        if(colectivoCreador.comprobarCreador(usuarioActual.getNombreUsuario())){
            proyectos.add(new Infraestructura(titulo, descripcion, importe, colectivoCreador, fecha, distritos, esquemaGrafico));
            try {
                this.guardarDatos();
            } catch (IOException e) {
                System.out.println("Error al guardar el estado del sistema");
            }
        }
        else{
            throw new ProyectoExcepcion("Usuario creador no se corresponde con el representante del grupo");
        }
    }

    /**
     * Este metodo devolvera los proyectos aceptados de la aplicacion
     * @return ArrayList que contiene todos los proyectos aceptados
     */
    public ArrayList<Proyecto> getProyectosAceptados(){
        ArrayList<Proyecto> listado = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.isAceptado())
                listado.add(p);
        }
        return listado;
    }

    /**
     * Este metodo hace que ek usuario actual siga un colectivo
     * @param colectivo Colectivo que se va a seguir
     * @throws AplicacionExcepcion Si se produce algun error al seguir al colectivo
     */
    public void seguirColectivo(Colectivo colectivo) throws AplicacionExcepcion{
        if(comprobarSeguir(colectivo))
            colectivo.seguirColectivo(usuarioActual);
        else
            throw new AplicacionExcepcion("No se puede seguir al colectivo");
    }

    /**
     * Este metodo comprueba si el usuario que se encuentra en la aplicacion puede
     * seguir al colectivo en cuestion
     * @param colectivo Colectivo que se desea comprobar
     * @return boolean: true si lo puede seguir o false si no
     */
    public boolean comprobarSeguir(Colectivo colectivo){
        ArrayList<Colectivo> colectivosSeguidos = getColectivosSeguidos();

        for(Colectivo c : colectivosSeguidos){
            // Si el usuario ya sigue el colectivo
            if(c.equals(colectivo)){
                return false;
            }
        }

        for(Colectivo c : getColectivos()){
            for(Colectivo c_seg : colectivosSeguidos){
                // Comprobamos si alguno de los colectivos seguidos es hijo de otro colectivo
                if(c.comprobarNombre(c_seg.getNombre())){
                    // Si alguno de los colectivos seguidos es hijo pasamos a comprobar
                    // desde el padre si el usuario ya sigue alguno de los colectivos hijos
                    // Si el padre contiene el colecitvo que se va a seguir el usuario no podra seguirlo
                    if(c.comprobarNombre(colectivo.getNombre()))
                        return false;
                }
            }
        }
        return true;
    }

    /**
     * Este metodo devolvera los proyectos caducados de la aplicacion
     * @return ArrayList que contiene todos los proyectos caducados
     */
    public ArrayList<Proyecto> getProyectosCaducados(){
        ArrayList<Proyecto> listado = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.isCaducado())
                listado.add(p);
        }
        return listado;
    }

    /**
     * Este metodo devolvera los proyectos pendientes de la aplicacion
     * @return ArrayList que contiene todos los proyectos pendientes
     */
    public ArrayList<Proyecto> getProyectosPendientes(){
        ArrayList<Proyecto> listado = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.isPendiente())
                listado.add(p);
        }
        return listado;
    }

    /**
     * Este metodo devolvera los proyectos que pueden solicitar financiacion de la aplicacion
     * @return ArrayList que contiene todos los proyectos que pueden solicitar financiacion
     */
    public ArrayList<Proyecto> getProyectosSolicitable(){
        ArrayList<Proyecto> listado = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.isSolicitable())
                listado.add(p);
        }
        return listado;
    }

    /**
     * Este metodo devolvera los proyectos disponibles de la aplicacion, los que no han sido rechazados y no estan pendientes de
     * aceptacion
     * @return ArrayList que contiene todos los proyectos disponibles
     */
    public ArrayList<Proyecto> getProyectosDisponibles() {
        ArrayList<Proyecto> proyectosDisponibles = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(!p.isRechazado() && !p.isPendiente())
                proyectosDisponibles.add(p);
        }
        return proyectosDisponibles;
    }


    /**
     * Este metodo devolvera los proyectos rechazados de la aplicacion
     * @return ArrayList que contiene todos los proyectos rechazados
     */
    public ArrayList<Proyecto> getProyectosRechazados() {
        ArrayList<Proyecto> proyectosRechazados = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.isRechazado())
                proyectosRechazados.add(p);
        }
        return proyectosRechazados;
    }

    /**
     * Este metodo devuelve los proyectos en espera de financiacion en la apliacion
     * @return ArrayList que contiene los proyectos en espeara de financiacion
     */
    public ArrayList<Proyecto> getProyectosEspera(){
        ArrayList<Proyecto> proyectosEspera = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.isEspera())
                proyectosEspera.add(p);
        }
        return proyectosEspera;
    }

    /**
     * Este metodo devuelve los proyectos que han sido financiados en la aplicacion
     * @return ArrayList que contiene los proyectos financiados en la aplicacion
     */
    public ArrayList<Proyecto> getProyectosFinanciados(){
        ArrayList<Proyecto> proyectosFinanciados = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.isFinanciado())
                proyectosFinanciados.add(p);
        }
        return proyectosFinanciados;
    }

    /**
     * Este metodo devuelve los usuarios que han sido aceptados en la aplicacion
     * @return ArrayList que contiene los usuarios que han sido aceptados en la aplicacion
     */
    public ArrayList<UsuarioRegistrado> getUsuariosAceptados(){
        ArrayList<UsuarioRegistrado> usuarios = new ArrayList<>();

        for(UsuarioRegistrado u : usuariosRegistrados){
            if(!u.isBloqueado())
                usuarios.add(u);
        }
        return usuarios;
    }

    /**
     * Este metodo devuelve los usarios que han sido bloqueados en la aplicacion
     * @return ArrayList que contiene los usuarios bloqueados en la aplicacion
     */
    public ArrayList<UsuarioRegistrado> getUsuariosRechazados(){
        ArrayList<UsuarioRegistrado> usuarios = new ArrayList<>();

        for(UsuarioRegistrado u : usuariosRegistrados){
            if(u.isBloqueado())
                usuarios.add(u);
        }
        return usuarios;
    }

    /**
     * Este metodo comprobara la caducidad de los proyectos en la aplicacion
     */
    public void comprobarCaducados() {
        long dias;

        for (Proyecto p : proyectos) {
            if(!p.isRechazado()){
                dias = ChronoUnit.DAYS.between(p.getFechaUltimoVoto(), FechaSimulada.getHoy());

                if (dias > 30 && !p.isCaducado()) {
                    p.caducado();
                }
            }
        }

        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }


    /**
     * Este metodo comprobara si los proyectos de la aplicacion han llegado al numero de votos establecido
     */
    public void comprobarVotos() {
        for (Proyecto p : proyectos) {
            if (p.isAceptado()) {
                if (p.getVotos() > numero_votos)
                    p.solicitable();
            }
            else if(p.isSolicitable() && p.getVotos() < numero_votos)
                p.aceptado();
        }
        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }

    /**
     * Este metodo comprobara si los proyectos que han mandado peticion de financiacion se les ha concedido
     * el importe o han sido rechazados
     */
    public void comprobarProyectosEspera(){
        for(Proyecto p : proyectos){
            if(p.isEspera()){
                try {
                    comprobarFinanciacionProyecto(p);
                } catch (AplicacionExcepcion | IOException | InvalidIDException ignored) {

                }
            }
        }
    }

    /**
     * Este metodo realiza la comprobacion de votos, caducidad y de proyectos en espera de la aplicacion
     * Normalmente se ejecutara al cargar la aplicacion
     */
    private void comprobaciones(){
        comprobarVotos();
        comprobarCaducados();
        comprobarProyectosEspera();
    }

    /**
     * Este metodo devolver los colectivos seguidos por el usuario que esta usando la aplicacion
     * @return ArrayList que contiene todos los colectivos que sigue el usuario
     */
    public ArrayList<Colectivo> getColectivosSeguidos() {
        ArrayList<Colectivo> colectivosSeguidos = new ArrayList<>();
        for(Colectivo c : getColectivos()){
            if(c.comprobarUsuario(usuarioActual))
                colectivosSeguidos.add(c);
        }

        return colectivosSeguidos;
    }

    /**
     * Este metodo devolver los proyectos seguidos por el usuario que esta usando la aplicacion
     * @return ArrayList que contiene todos los proyectos que sigue el usuario
     */
    public ArrayList<Proyecto> getProyectosSeguidos() {
        ArrayList<Proyecto> proyectosSeguidos = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.comprobarSeguidor(usuarioActual))
                proyectosSeguidos.add(p);
            ArrayList<Colectivo> colectivosSeguidos = p.getColectivosSeguidores();
            for(Colectivo c : colectivosSeguidos){
                if(c.comprobarUsuario(usuarioActual))
                    proyectosSeguidos.add(p);
            }
        }

        return proyectosSeguidos;
    }

    /**
     * Este metodo devuelve los proyectos seguidos por un colectivo
     * @return ArrayList que contiene todos los colectivos que sigue el colectivo
     */
    public ArrayList<Proyecto> getProyectosSeguidos(Colectivo colectivo) {
        ArrayList<Proyecto> proyectosSeguidos = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.comprobarColectivoSeguidor(colectivo))
                proyectosSeguidos.add(p);
        }

        return proyectosSeguidos;
    }

    /**
     * Getter de los colectivos creados por el usuario actual de la aplicacion
     * @return ArrayList que contiene los colectivos del usuario
     */
    public ArrayList<Colectivo> getMisColectivos(){
        ArrayList<Colectivo> misColectivos = new ArrayList<>();

        for(Colectivo c : getColectivos()){
            if(c.getRepresentante().equals(usuarioActual)){
                misColectivos.add(c);
            }
        }

        return misColectivos;
    }

    /**
     * Getter de los proyectos creados por el usuario actual de la aplicacion
     * @return ArrayList que contiene los proyectos del usuario
     */
    public ArrayList<Proyecto> getMisProyectos(){
        ArrayList<Proyecto> misProyectos = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.getCreador() != null){
                if(p.getCreador().equals(usuarioActual))
                    misProyectos.add(p);
            }
            else{
                if(p.getColectivoCreador().getRepresentante().equals(usuarioActual))
                    misProyectos.add(p);
            }

        }

        return misProyectos;
    }

    /**
     * Getter de los proyectos que ha votado el usuario actual de la aplicacion
     * @return ArrayList que contiene los proyectos votados por el usuario
     */
    public ArrayList<Proyecto> getMisVotados(){
        ArrayList<Proyecto> misVotados = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.comprobarVotante(usuarioActual))
                misVotados.add(p);
        }
        return misVotados;
    }

    /**
     * Getter de los proyectos creados por un colectivo
     * @param colectivo Colectivo creador de los proyectos
     * @return ArrayList que contiene los proyectos creados
     */
    private ArrayList<Proyecto> getProyectosCreados(Colectivo colectivo){
        ArrayList<Proyecto> proyectosCreados = new ArrayList<>();

        for(Proyecto p : proyectos){
            if(p.getCreador() == null){
                if(p.getColectivoCreador().equals(colectivo))
                    proyectosCreados.add(p);
            }
        }
        return proyectosCreados;
    }

    /**
     * Este metodo calcula la afinidad de dos grupos seguidos por el usuario que esta utilizando la aplicacion
     * @param a Colectivo A que sigue el usuario
     * @param b Colectivo B que sigue el usuario
     * @return Valor que representa la afinidad entre los dos colectivos
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error al obtener la afinidad
     */
    public double consultarAfinidad(Colectivo a, Colectivo b) throws AplicacionExcepcion{
        double contador = 0;

        if(!a.comprobarUsuario(usuarioActual) || !a.comprobarUsuario(usuarioActual))
            throw new AplicacionExcepcion("El usuario debe pertenecer a los dos colectivos para consultar afinidad");

        ArrayList<Proyecto> proyectos_seguidos_a = getProyectosSeguidos(a);
        ArrayList<Proyecto> proyectos_seguidos_b = getProyectosSeguidos(b);
        ArrayList<Proyecto> proyectos_creados_a = getProyectosCreados(a);
        ArrayList<Proyecto> proyectos_creados_b = getProyectosCreados(b);
        if(proyectos_creados_a.size() == 0 && proyectos_creados_b.size() == 0)
            return 0;

        for(Proyecto pa : proyectos_creados_a){
            for(Proyecto pb : proyectos_seguidos_b){
                if(pa.getUniqueId().equals(pb.getUniqueId()))
                    contador++;
            }
        }

        for(Proyecto pb : proyectos_creados_b){
            for(Proyecto pa : proyectos_seguidos_a){
                if(pa.getUniqueId().equals(pb.getUniqueId()))
                    contador++;
            }
        }

        return (contador * 2)/proyectos_creados_a.size() + proyectos_creados_b.size();
    }

    /**
     * Metodo que manda un proyecto a la API de financiacion
     * @param proyecto Proyecto que se manda a financiacion
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion
     * @throws IOException Esta excepcion se lanzara si se produce un error en la API
     * @throws InvalidRequestException Esta excepcion se lanzara si la peticion a la API esta mal formada
     */
    public void solicitarFinanciacion(Proyecto proyecto) throws AplicacionExcepcion, IOException, InvalidRequestException {
        if (proyecto.comprobarCreador(usuarioActual)) {
            if (proyecto.isSolicitable()) {
                proyecto.espera();
                // Llamamos a la API que pide financiacion
                String id = realizarSolicitudFinanciacion(proyecto, FechaSimulada.getHoy());
                proyecto.peticion(id);

                try {
                    this.guardarDatos();
                } catch (IOException e) {
                    System.out.println("Error al guardar el estado del sistema");
                }

            } else {
                throw new AplicacionExcepcion("El proyecto aun no ha alcanzado los votos necesarios");
            }
        } else {
            throw new AplicacionExcepcion("El solicitante no es el creador del proyecto");
        }
    }

    /**
     * Este metodo mandara un proyecto a la API de financiacion
     * Este metodo esta creado principalmente por motivos de test
     * @param proyecto Proyecto que se mandara a la API
     * @param fecha Fecha en la que se manda el proyecto
     * @return String que contiene el id de la solicitud de financiacion
     * @throws IOException Esta excepcion se lanzara si se produce un error en la API
     * @throws InvalidRequestException Esta excepcion se lanzara si la peticion a la API esta mal formada
     */
    private String realizarSolicitudFinanciacion(Proyecto proyecto, LocalDate fecha) throws IOException, InvalidRequestException {
        CCGG proxy = CCGG.getGateway();
        proxy.setDate(fecha);
        return proxy.submitRequest(proyecto);
    }

    /**
     * Este metodo comprueba el estado de la peticion a la API de financiacion de un proyecto
     * @param proyecto Proyecto del que se realiza la consulta
     */
    public void comprobarFinanciacionProyecto(Proyecto proyecto) throws AplicacionExcepcion, IOException, InvalidIDException {
        if (!proyecto.isEspera())
            return;
        Object importeConcedido = comprobarFinanciacion(proyecto.getPeticion(), FechaSimulada.getHoy());
        if (importeConcedido == null) {
            throw new AplicacionExcepcion("Proyecto pendiente de financiacion");
        }
        if ((double) importeConcedido == 0) {
            proyecto.aceptado();
            throw new AplicacionExcepcion("El proyecto no ha sido financiado");
        } else {
            proyecto.financiado((double) importeConcedido);
        }
    }

    /**
     * Este metodo comprueba el estado de financiacion de un proyecto dado un id
     * Este metodo esta creado principalmente por motivos de test
     * @param id String que contiene el id de la peticion
     * @param fecha LocalDate que contiene la fecha en la que se realiza la consulta
     * @return Importe concedido al proyecto o null si aun no se ha atendido la peticion
     * @throws IOException Esta excepcion se lanzara si se produce un error en la API
     * @throws InvalidIDException Esta excepcion se lanzara si el id de solicitud no se corresponde con ninguna solicitud
     * realizada
     */
    private Object comprobarFinanciacion(String id, LocalDate fecha) throws IOException, InvalidIDException {
        CCGG proxy = CCGG.getGateway();
        proxy.setDate(fecha);
        return proxy.getAmountGranted(id);
    }

    /**
     * Este metodo permite a un administrador bloquear a un usuario en la aplicacion
     * @param usuario UsuarioRegistrado que sera bloqueado
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al realizar el bloqueo
     */
    public void bloquearUsuario(UsuarioRegistrado usuario) throws AplicacionExcepcion{
        if(admin == null){
            throw new AplicacionExcepcion("Accion no permitida en la aplicacion");
        }

        for(UsuarioRegistrado u : usuariosRegistrados){
            if(u.getNombreUsuario().equals(usuario.getNombreUsuario())){
                u.bloqueado();
                try {
                    guardarDatos();
                } catch (IOException e) {
                    System.out.println("Error al guardar el estado del sistema");
                }
                return;
            }
        }
        throw new AplicacionExcepcion("No existe el usuario en la aplicacion");
    }

    /**
     * Este metodo permite a un administrador desbloquear a un usuario en la aplicacion
     * @param usuario UsuarioRegistrado que sera desbloqueado
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al realizar el desbloqueo
     */
    public void desbloquearUsuario(UsuarioRegistrado usuario) throws AplicacionExcepcion{
        for(UsuarioRegistrado u : usuariosRegistrados){
            if(u.getNombreUsuario().equals(usuario.getNombreUsuario())){
                u.aceptado();
                try {
                    this.guardarDatos();
                } catch (IOException e) {
                    System.out.println("Error al guardar el estado del sistema");
                }
                return;
            }
        }
        throw new AplicacionExcepcion("No existe el usuario en la aplicacion");
    }

    /**
     * Este metodo devuelve las notificaciones del usuario que se encuentra utilizando la aplicacion
     * @return ArrayList que contiene las notificaciones del usuario
     */
    public ArrayList<String> getNotificaciones() {
        return usuarioActual.getNotificaciones();
    }

    /**
     * Este metodo permite a un administrador rechazar un proyecto de la aplicacion
     * @param proyecto Proyecto que va a ser rechazado
     * @param motivo String que contiene el motivo del rechazo
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al rechazar el proyecto
     */
    public void rechazarProyecto(Proyecto proyecto, String motivo) throws AplicacionExcepcion{
        for(Proyecto p : proyectos){
            if(p.getUniqueId().equals(proyecto.getUniqueId())) {
                p.rechazado(motivo);
                try {
                    this.guardarDatos();
                } catch (IOException e) {
                    System.out.println("Error al guardar el estado del sistema");
                }
                return;
            }

        }
        throw new AplicacionExcepcion("No existe el proyecto en la aplicacion");
    }

    /**
     * Este metodo permite a un administrador aceptar un proyecto de la aplicacion
     * @param proyecto Proyecto que va a ser aceptado
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al aceptar el proyecto
     */
    public void aceptarProyecto(Proyecto proyecto) throws AplicacionExcepcion{
        for(Proyecto p : proyectos){
            if(p.getUniqueId().equals(proyecto.getUniqueId())) {
                p.aceptado();
                try {
                    this.guardarDatos();
                } catch (IOException e) {
                    System.out.println("Error al guardar el estado del sistema");
                }
                return;
            }
        }
        throw new AplicacionExcepcion("No existe el proyecto en la aplicacion");
    }

    /**
     * Este metodo permite al usuario que esta utilizando la aplicacion votar un proyecto
     * @param proyecto Proyecto que sera votado por el usuario
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al votar el proyecto
     */
    public void votarProyecto(Proyecto proyecto) throws AplicacionExcepcion{
        if(proyecto.isCaducado() || proyecto.isRechazado()){
            throw new AplicacionExcepcion("El estado del proyecto no permite que este sea votado");
        }
        proyecto.votarProyecto(usuarioActual);
        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }
    }

    /**
     * Este metodo permite a un colectivo votar un proyecto
     * @param colectivo Colectivo que va a votar el proyecto
     * @param proyecto Proyecto que sera votado por el colectivo
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al votar el proyecto
     */
    public void votarProyecto(Colectivo colectivo, Proyecto proyecto) throws AplicacionExcepcion{
        if(colectivo.comprobarCreador(usuarioActual.getNombreUsuario())){
            if(proyecto.isCaducado() || proyecto.isRechazado()){
                throw new AplicacionExcepcion("El estado del proyecto no permite que este sea votado");
            }
            proyecto.votarProyecto(colectivo);
            try {
                this.guardarDatos();
            } catch (IOException e) {
                System.out.println("Error al guardar el estado del sistema");
            }
        }
        else{
            throw new AplicacionExcepcion("El usuario no es el representante del colectivo");
        }

    }

    /**
     * Este metodo permite al administrador settear el numero de votos necesarios para mandar un proyecto a financiacion
     * @param num_votos Numero de votos que se va a settear
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al settear el numero de votos
     */
    public void setNumero_votos(int num_votos) throws AplicacionExcepcion {
        if(numero_votos < 0)
            throw new AplicacionExcepcion("El numero de votos no puede ser negativo");

        numero_votos = num_votos;

        try {
            this.guardarDatos();
        } catch (IOException e) {
            System.out.println("Error al guardar el estado del sistema");
        }

        comprobarVotos();
    }

    /**
     * Esta funcion permite guardar usuarios en la aplicacion a partir de un fichero
     * Este metodo esta creado con motivos de test
     * @param nombreFichero String que contiene el nombre del fichero que contiene los usuarios
     * @throws FileNotFoundException Esta excepcion se lanzara si no existe el fichero de usuarios
     * @throws ErrorFichero Esta excepcion se lanzara si el fichero de usuarios no tiene el formato correcto
     */
    public void leerFicheroDatosUsuarios(String nombreFichero) throws FileNotFoundException, ErrorFichero {

        if(nombreFichero.isEmpty()) return;

        Scanner scanner = new Scanner(new File(nombreFichero));
        scanner.useDelimiter(";");

        String line, dni, nombreUsuario, contrasenia, checkeo;

        try{
            while((line = scanner.nextLine()) != null){
                String[] values = line.split(";");

                if(values.length != 3)
                    throw new ErrorFichero("Numero incorrecto de separadores");
                checkeo = dni = values[0];
                if(checkeo.trim().isEmpty())
                    throw new ErrorFichero("Campos vacios en el fichero");
                checkeo = nombreUsuario = values[1];
                if(checkeo.trim().isEmpty())
                    throw new ErrorFichero("Campos vacios en el fichero");
                checkeo = contrasenia = values[2];
                if(checkeo.trim().isEmpty())
                    throw new ErrorFichero("Campos vacios en el fichero");

                try{
                    registrar(nombreUsuario, dni, contrasenia);
                } catch (UsuarioExcepcion usuarioExcepcion) {
                    System.out.println(usuarioExcepcion);
                }
            }
        } catch (NoSuchElementException ignored){

        }
    }

    /**
     * Este metodo permite al usuario que esta utilizando la aplicacion limpiar sus notificaciones
     */
    public void limpiarNotificacionesUsuario() {
        usuarioActual.limpiarNotificaciones();
    }

    /**
     * Este metodo devuelve un colectivo con un determinado nombre contenido en la aplicacion
     * @param nombre String que contiene el nombre del colectivo
     * @return Colectivo con el nombre dado o null si no se encuentra
     */
    public Colectivo getColectivo(String nombre){
        for(Colectivo c : colectivos){
            if(c.getNombre().equals(nombre))
                return c;
        }
        return null;
    }

    /**
     * Este metodo devuelve un proyecto con un determinado id contenido en la aplicacion
     * @param id Id del proyecto
     * @return Proyecto con el id dado o null si no se encuentra
     */
    public Proyecto getProyecto(UUID id){
        for(Proyecto p : proyectos){
            if(p.getUniqueId().equals(id))
                return p;
        }
        return null;
    }

    /**
     * Getter de un proyecto en un indice del ArrayList de proyectos de la aplicacion
     * @param i Indice en el que se encuentra el proyecto
     * @return Proyecto que se corresponde a ese indice
     * @throws AplicacionExcepcion Esta excepcion se lanzara si se produce un error en la aplicacion al obtener el proyecto
     */
    public Proyecto getProyecto_i(int i) throws AplicacionExcepcion {
        if(i >= proyectos.size())
            throw new AplicacionExcepcion("No existe el proyecto");
        return proyectos.get(i);
    }

    public Colectivo getColectivo_i(int i) throws AplicacionExcepcion {
        if(i >= colectivos.size())
            throw new AplicacionExcepcion("No existe el colectivo");
        return colectivos.get(i);
    }

    // Getters y setters

    /**
     * Getter de los colectivos de la aplicacion
     * @return ArrayList que contiene todos los colectivos de la aplicacion
     */
    public ArrayList<Colectivo> getColectivos() {
        ArrayList<Colectivo> colectivos_list = new ArrayList<>();

        for(Colectivo c : colectivos){
            colectivos_list.add(c);
            for(Colectivo c_hijo : c.getColectivosHijo()){
                colectivos_list.addAll(getColectivos_recursivo(c_hijo));
            }
        }
        return colectivos_list;
    }

    /**
     * Metodo recursivo del metodo getColectivo
     * @param colectivo Colectivo sobre el que se aplica la recursividad
     * @return ArrayList que contiene el listado de colectivo
     */
    private ArrayList<Colectivo> getColectivos_recursivo(Colectivo colectivo){
        ArrayList<Colectivo> colectivos_list = new ArrayList<>();
        colectivos_list.add(colectivo);
        for(Colectivo c : colectivo.getColectivosHijo()){
            colectivos_list.addAll(getColectivos_recursivo(c));
        }
        return colectivos_list;
    }

    /**
     * Getter de los proyectos de la aplicacion
     * @return ArrayList que contiene todos los proyectos de la aplicacion
     */
    public ArrayList<Proyecto> getProyectos() {
        return proyectos;
    }

    /**
     * Getter del numero de votos limite para mandar proyecto a financiacion
     * @return Numero de votos para mandar a financiacion
     */
    public int getNumero_votos() {
        return numero_votos;
    }

    /**
     * Getter de los usuarios registrados de la aplicacion
     * @return ArrayList que contiene todos los usuarios de la aplicacion
     */
    public ArrayList<UsuarioRegistrado> getUsuariosRegistrados() {
        return usuariosRegistrados;
    }

    /**
     * Getter del atributo usuarioActual de la aplicacion
     * @return UsuarioRegistrado que esta utilizando la aplicacion
     */
    public UsuarioRegistrado getUsuarioActual(){
        return usuarioActual;
    }

    /**
     * Getter del atributo admin de la aplicacion
     * @return Admin que esta utilizando la aplicacion
     */
    public Admin getAdmin(){
        return admin;
    }

    /**
     * Setter del atributo proyectos de la aplicacion
     * @param proyectos ArrayList que contiene todos los proyectos que se van a settear en la aplicacion
     */
    public void setProyectos(ArrayList<Proyecto> proyectos){
        this.proyectos = proyectos;
    }

    /**
     * Setter del atributo colectivos de la aplicacion
     * @param colectivos ArrayList que contiene todos los proyectos que se van a settear en la aplicacion
     */
    public void setColectivos(ArrayList<Colectivo> colectivos){
        this.colectivos = colectivos;
    }

    /**
     * Setter del atributo usuariosRegistrados de la aplicacion
     * @param usuariosRegistrados ArrayList que contiene todos los usuarios que se van a settear en la aplicacion
     */
    public void setUsuariosRegistrados(ArrayList<UsuarioRegistrado> usuariosRegistrados){
        this.usuariosRegistrados = usuariosRegistrados;
    }

    /**
     * Setter del atributo numero_votos de la aplicacion
     * @param votos Numero de votos que se van a settear en la aplicacion
     */
    public void setVotos(int votos){
        numero_votos = votos;
    }

    /**
     * Setter del atributo usuarioActual de la aplicacion
     * @param usuarioActual UsuarioRegistrado que se va a settear en la aplicacion
     */
    public void setUsuarioActual(UsuarioRegistrado usuarioActual){
        this.usuarioActual = usuarioActual;
    }
}
