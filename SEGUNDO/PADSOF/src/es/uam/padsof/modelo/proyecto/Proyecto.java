package es.uam.padsof.modelo.proyecto;

import es.uam.eps.sadp.grants.GrantRequest;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.ProyectoExcepcion;
import es.uam.padsof.modelo.fechasimulada.FechaSimulada;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.io.Serializable;
import java.text.DecimalFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Observable;
import java.util.UUID;

/**
 * Clase abstracta Proyecto que contiene toda la informacion acerca de un proyecto
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public abstract class Proyecto extends Observable implements Serializable, GrantRequest {
    /**
     * Serial Version para serializar la aplicacion
     */
    private static final long serialVersionUID = 1L;

    /**
     * Nombre del proyecto
     */
    private String titulo;

    /**
     * Descripcion del proyecto
     */
    private String descripcion;

    /**
     * String que contiene el id del proyecto al hacer peticion de financiacion
     */
    private String peticion;

    /**
     * Importe que se solicita en el proyecto
     */
    private double importe;

    /**
     * Importe concedido en el proyecto
     */
    private double importeConcedido;

    /**
     * UsuarioRegistrado creador del proyecto
     */
    private UsuarioRegistrado creador;

    /**
     * Colectivo creador del proyecto
     */
    private Colectivo colectivoCreador;

    /**
     * ArrayList que contiene los usuarios que siguen el proyecto
     */
    private ArrayList<UsuarioRegistrado> seguidores;

    /**
     * ArrayList que contiene los colectivos que siguen el proyecto
     */
    private ArrayList<Colectivo> colectivosSeguidores;

    /**
     * ArrayList que contiene los usuario que han votado el proyecto
     */
    private ArrayList<UsuarioRegistrado> votantes;

    /**
     * EstadoProyecto que contiene informacion acerca del estado del proyecto en la aplicacion
     */
    private EstadoProyecto estado;

    /**
     * Motivo por el cual un projecto ha sido rechazado
     */
    private String motivoRechazo;

    /**
     * LocalDate que contiene la fecha en la que se creo el proyecto
     */
    private LocalDate fechaProyecto;

    /**
     * LocalDate que contiene la fecha del utlimo voto del proyecto
     */
    private LocalDate fechaUltimoVoto;

    /**
     * Id unico del proyecto en la aplicacion
     */
    private UUID uniqueId;

    /**
     * Constructor de la clase Proyecto
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param creador Usuario creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     */
    Proyecto(String titulo, String descripcion, double importe, UsuarioRegistrado creador, LocalDate fecha){
        this.titulo = titulo;
        this.descripcion = descripcion;
        this.importe = importe;
        this.creador = creador;
        this.peticion = null;
        this.seguidores = new ArrayList<>();
        //this.seguidores.add(creador);
        this.colectivosSeguidores = new ArrayList<>();
        this.votantes = new ArrayList<>();
        votarProyecto(creador);
        this.estado = EstadoProyecto.PENDIENTE;
        this.fechaProyecto = fecha;
        this.fechaUltimoVoto = fecha;
        this.uniqueId = UUID.randomUUID();
        this.importeConcedido = 0;
        this.colectivoCreador = null;
    }

    /**
     * Constructor de la clase Proyecto
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param colectivoCreador Colectivo creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     */
    Proyecto(String titulo, String descripcion, double importe, Colectivo colectivoCreador, LocalDate fecha){
        this.titulo = titulo;
        this.descripcion = descripcion;
        this.importe = importe;
        this.creador = null;
        this.colectivoCreador = colectivoCreador;
        this.peticion = null;
        this.seguidores = new ArrayList<>();
        this.colectivosSeguidores = new ArrayList<>();
        //this.colectivosSeguidores.add(colectivoCreador);
        this.votantes = new ArrayList<>();
        votarProyecto(colectivoCreador);
        this.estado = EstadoProyecto.PENDIENTE;
        this.fechaProyecto = fecha;
        this.fechaUltimoVoto = fecha;
        this.uniqueId = UUID.randomUUID();
        this.importeConcedido = 0;
    }

    /**
     * Metodo para seguir al proyecto siendo un usuario
     * @param usuario UsuarioRegistrado que sigue el proyecto
     * @return boolean: true si el usuario a podido seguir el proyecto y false si no
     */
    public boolean seguirProyecto(UsuarioRegistrado usuario){
        if(!comprobarSeguidor(usuario) && !usuario.isBloqueado()){
            seguidores.add(usuario);
            return true;
        }
        return false;
    }

    /**
     * Metodo para seguir al proyecto siendo un colectivo
     * @param colectivo Colectivo que sigue el proyecto
     * @return boolean: true si el usuario a podido seguir el proyecto y false si no
     */
    public boolean seguirProyecto(Colectivo colectivo){
        if(!comprobarColectivoSeguidor(colectivo)){
            colectivosSeguidores.add(colectivo);
            return true;
        }
        return false;
    }

    /**
     * Metodo para abandonar un proyecto siendo un usuario
     * @param usuario UsuarioRegistrado que abandona el proyecto
     * @return boolean: true si ha podido abandonarlo o false si no
     */
    public boolean abandonarProyecto(UsuarioRegistrado usuario){
        for(UsuarioRegistrado u: seguidores){
            if(usuario.getNombreUsuario().equals(u.getNombreUsuario())){
                seguidores.remove(u);
                return true;
            }
        }
        return false;
    }

    /**
     * Metodo para abandonar un proyecto siendo un colectivo
     * @param colectivo Colectivo que abandona el proyecto
     * @return boolean: true si ha podido abandonarlo o false si no
     */
    public boolean abandonarProyecto(Colectivo colectivo){
        for(Colectivo c : colectivosSeguidores){
            if(c.getNombre().equals(colectivo.getNombre())){
                colectivosSeguidores.remove(c);
                return true;
            }
        }
        return false;
    }

    /**
     * Metodo que comprueba si un usuario sigue el proyecto
     * @param usuario UsuarioRegistrado que se va a comprobar
     * @return boolean: true si el usuario sigue el proyecto o false si no
     */
    public boolean comprobarSeguidor(UsuarioRegistrado usuario){
        for(UsuarioRegistrado u : seguidores){
            if(u.getNombreUsuario().equals(usuario.getNombreUsuario())){
                return true;
            }
        }
        return false;
    }

    /**
     * Metodo que comprueba si un colectivo sigue el proyecto
     * @param colectivo Colectivo que se va a comprobar
     * @return boolean: true si el colectivo sigue el proyecto o false si no
     */
    public boolean comprobarColectivoSeguidor(Colectivo colectivo){
        for(Colectivo c : colectivosSeguidores){
            if(c.getNombre().equals(colectivo.getNombre()))
                return true;
        }
        return false;
    }

    /**
     * Metodo que comprueba si un usuario es el creador del proyecto
     * @param usuario UsuarioRegistrado que se comprueba
     * @return boolean: true si el usuario es el creador o false si no
     */
    public boolean comprobarCreador(UsuarioRegistrado usuario) {
        if(creador != null)
            return usuario.getNombreUsuario().equals(creador.getNombreUsuario());
        return usuario.getNombreUsuario().equals(colectivoCreador.getRepresentante().getNombreUsuario());
    }

    /**
     * Metodo que cambia el estado de un proyecto a rechazado
     * @param motivo String que contiene el motivo del rechazo
     * @throws ProyectoExcepcion Exception que se lanza si el motivo del rechazo tiene mas de 50 caracteres
     */
    public void rechazarProyecto(String motivo) throws ProyectoExcepcion{
        if(motivo.length() > 50)
            throw new ProyectoExcepcion("El motivo de rechazo del proyecto no puede tener mas de 50 caracteres");
        this.motivoRechazo = motivo;
        this.estado = EstadoProyecto.RECHAZADO;
    }

    /**
     * Metodo que vota el proyecto siendo un usuario registrado
     * @param usuario UsuarioRegistrado que vota el proyecto
     */
    public void votarProyecto(UsuarioRegistrado usuario){
        if(usuario.isBloqueado())
            return;

        for(UsuarioRegistrado u : votantes){
            if(u.getNombreUsuario().equals(usuario.getNombreUsuario()))
                return;
        }
        fechaUltimoVoto = FechaSimulada.getHoy();
        votantes.add(usuario);
    }

    /**
     * Metodo que vota el proyecto siendo un colectivo
     * @param colectivo Colectivo que vota el proyecto
     */
    public void votarProyecto(Colectivo colectivo){
        ArrayList<UsuarioRegistrado> votantesColectivo = colectivo.getParticipantes();

        for(UsuarioRegistrado anadir : votantesColectivo){
            this.votarProyecto(anadir);
        }
    }

    /**
     * Metodo que cuenta los votos del proyecto
     * @return Numero de votos del proyecto
     */
    public int getVotos(){
        int votos = 0;

        for(UsuarioRegistrado u : votantes){
            if(!u.isBloqueado())
                votos++;
            else
                votantes.remove(u);
        }
        return votos;
    }

    /**
     * Metodo que comprueba si un usuario ha votado un proyecto
     * @param usuarioRegistrado Usuario registrado que se va a comprobar
     * @return boolean: true si el usuario ha votado el proyecto y false si no
     */
    public boolean comprobarVotante(UsuarioRegistrado usuarioRegistrado){
        for(UsuarioRegistrado u : votantes){
            if(u.equals(usuarioRegistrado))
                return true;
        }
        return false;
    }

    /**
     * Metodo que comprueba si el estado del proyecto es ACEPTADO
     * @return boolean: true si el estado se corresponde o false si no
     */
    public boolean isAceptado(){
        return estado == EstadoProyecto.ACEPTADO;
    }

    /**
     * Metodo que comprueba si el estado del proyecto es PENDIENTE
     * @return boolean: true si el estado se corresponde o false si no
     */
    public boolean isPendiente(){
        return estado == EstadoProyecto.PENDIENTE;
    }

    /**
     * Metodo que comprueba si el estado del proyecto es RECHAZADO
     * @return boolean: true si el estado se corresponde o false si no
     */
    public boolean isRechazado(){
        return estado == EstadoProyecto.RECHAZADO;
    }

    /**
     * Metodo que comprueba si el estado del proyecto es CADUCADO
     * @return boolean: true si el estado se corresponde o false si no
     */
    public boolean isCaducado(){
        return estado == EstadoProyecto.CADUCADO;
    }

    /**
     * Metodo que comprueba si el estado del proyecto es FINANCIADO
     * @return boolean: true si el estado se corresponde o false si no
     */
    public boolean isFinanciado(){
        return estado == EstadoProyecto.FINANCIADO;
    }

    /**
     * Metodo que comprueba si el estado del proyecto es ESPERA
     * @return boolean: true si el estado se corresponde o false si no
     */
    public boolean isEspera(){
        return estado == EstadoProyecto.ESPERA;
    }

    /**
     * Metodo que comprieba si el estado del proyecto es SOLICTABLE
     * @return boolean: true si el estado se corresponde o false si no
     */
    public boolean isSolicitable(){
        return estado == EstadoProyecto.SOLICITABLE;
    }

    /**
     * Metodo que cambia el estado de un proyecto a ESPERA y notifica a los seguidores del mismo
     */
    public void espera(){
        estado = EstadoProyecto.ESPERA;

        String notificacion = "El estado del proyecto '" + titulo + "' a pasado a espera de finanaciacion";

        this.notificar(notificacion);
    }

    /**
     * Metodo que cambia el estado de un proyecto a ACEPTADO y notifica a los seguidores del mismo
     */
    public void aceptado(){
        estado = EstadoProyecto.ACEPTADO;

        String notificacion = "El estado del proyecto '" + titulo + "' a pasado a aceptado";

        this.notificar(notificacion);
    }

    /**
     * Metodo que cambia el estado de un proyecto a PENDIENTE y notifica a los seguidores del mismo
     */
    public void pendiente(){
        estado = EstadoProyecto.PENDIENTE;

        String notificacion = "El estado del proyecto '" + titulo + "' a pasado a pendiente";

        this.notificar(notificacion);
    }

    /**
     * Metodo que cambia el estado de un proyecto a RECHAZADO y notifica a los seguidores del mismo
     * @param motivoRechazo String que contiene el motivo de rechazo del proyecto
     */
    public void rechazado(String motivoRechazo){
        estado = EstadoProyecto.RECHAZADO;
        this.motivoRechazo = motivoRechazo;

        String notificacion = "El estado del proyecto '" + titulo + "' a pasado a rechazado por el siguiente motivo: '" + motivoRechazo + "'";

        this.notificar(notificacion);
    }

    /**
     * Metodo que cambia el estado de un proyecto a CADUCADO y notifica a los seguidores del mismo
     */
    public void caducado(){
        estado = EstadoProyecto.CADUCADO;

        String notificacion = "El estado del proyecto '" + titulo + "' a pasado a caducado";

        this.notificar(notificacion);
    }

    /**
     * Metodo que cambia el estado de un proyecto a FINANCIADO y notifica a los seguidores del mismo
     * @param importe Importe que ha sido concedido al proyecto
     */
    public void financiado(double importe){
        estado = EstadoProyecto.FINANCIADO;
        importeConcedido = importe;

        DecimalFormat df = new DecimalFormat("0.00");
        String notificacion = "El estado del proyecto '" + titulo + "' a pasado a financiado y el importe concedido es '" + df.format(importeConcedido) + "'";

        this.notificar(notificacion);
    }

    /**
     * Metodo que cambia el estado de un proyecto a SOLICITABLE y notifica a los seguidores del mismo
     */
    public void solicitable(){
        estado = EstadoProyecto.SOLICITABLE;

        String notificacion = "El estado del proyecto '" + titulo + "' a pasado a solicitable";

        this.notificar(notificacion);

    }

    /**
     * Metodo que cambia el estado de un proyecto a ESPERA y notifica a los seguidores del mismo
     * @param peticion String que contiene el id de la peticion de financiacion del proyecto
     */
    public void peticion(String peticion) {
        this.peticion = peticion;
        estado = EstadoProyecto.ESPERA;

        String notificacion = "El proyecto '" + titulo + "'esta siendo atendido para la concesion de financiacion";
        this.notificar(notificacion);
    }

    /**
     * Metodo que manda notificacion a todos los seguidores del proyecto acerca de los cambios producidos
     * en el mismo
     * @param notificacion String que contiene la notificacion que se mandara a los usuario
     */
    private void notificar(String notificacion){
        for(Colectivo c : colectivosSeguidores){
            c.update(this, notificacion);
        }

        for(UsuarioRegistrado u : seguidores){
            u.update(this, notificacion);
        }
    }

    /**
     * Metodo toString para representar el proyecto
     * @return String que contiene toda la informacion acerca del proyecto
     */
    @Override
    public String toString(){
        return titulo + "\t" + descripcion + "\tId: " + uniqueId + "\tImporte: " + importe + "\tImporte concedido: " + importeConcedido + "\tFecha: " + fechaProyecto +  "\tNumero de votos: " + getVotos() + "\tEstado: " + estado + "\tSeguidores: " + seguidores + "\tColectivos seguidores: " + colectivosSeguidores;
    }

    // Getters y setters

    /**
     * Getter del creador de un proyecto
     * @return UsuarioRegistrado que contiene el creador del grupo
     */
    public UsuarioRegistrado getCreador() {
        return creador;
    }

    /**
     * Getter de los seguidores de un proyecto
     * @return ArrayList que contiene los usuarios registrados que siguen un proyecto
     */
    public ArrayList<UsuarioRegistrado> getSeguidores() {
        return seguidores;
    }

    /**
     * Getter de los colectivos seguidores de un proyecto
     * @return ArrayList que contiene los colectivos que siguen un proyecto
     */
    public ArrayList<Colectivo> getColectivosSeguidores() {
        return colectivosSeguidores;
    }

    /**
     * Getter del estado del proyecto
     * @return Estado que contiene el estado actual del proyecto
     */
    public EstadoProyecto getEstado() {
        return estado;
    }

    /**
     * Getter que devuelve le motivo de rechazo de un proyecto
     * @return String que contiene el motivo de rechazo
     */
    public String getMotivoRechazo() {
        return motivoRechazo;
    }

    /**
     * Getter que devuelve el colectivo creador de un proyecto
     * @return Colectivo creador del proyecto
     */
    public Colectivo getColectivoCreador() {
        return colectivoCreador;
    }

    // Metodos de GrantRequest

    /**
     * Metodo que devuelve informacion adicional del proyecto
     * Metodo implementado de la clase GrantRequest
     * @return null: En las diferentes clases que heredan de Proyecto se devolvera la informacion correcta
     */
    public String getExtraData(){
        return null;
    }

    /**
     * Metodo que devuelve la descripcion del proyecto
     * Metodo implementado de la clase GrantRequest
     * @return String que contiene la descripcion del proyecto
     */
    public String getProjectDescription(){
        return descripcion;
    }

    /**
     * Metodo que devuelve el tipo de proyecto
     * Metodo implementado de la interfaz GrantRequest
     * @return null: En las diferentes clases que heredan de Proyecto se devolvera el tipo correcto
     */
    public GrantRequest.ProjectKind getProjectKind(){
        return null;
    }

    /**
     * Metodo que devuelve el nombre del proyecto
     * Metodo implementado de la clase GrantRequest
     * @return String que contiene el nombre del proyecto
     */
    public String getProjectTitle(){
        return titulo;
    }

    /**
     * Metodo que devuelve el importe solicitado en el proyecto
     * Metodo implementado de la clase GrantRequest
     * @return Importe que se solicita del proyecto
     */
    public double getRequestedAmount(){
        return importe;
    }

    // Getters y setters

    /**
     * Getter de la fecha del proyecto
     * @return LocalDate que contiene la fecha del proyecto
     */
    public LocalDate getFechaProyecto() {
        return fechaProyecto;
    }

    /**
     * Getter de la fecha del ultimo voto del proyecto
     * @return LocalDate que contiene la fecha en la que se hizo la ultima votacion
     */
    public LocalDate getFechaUltimoVoto(){
        return fechaUltimoVoto;
    }

    /**
     * Getter del identificador del proyecto
     * @return UUID que contiene la identificacion del proyecto
     */
    public UUID getUniqueId() {
        return uniqueId;
    }

    /**
     * Getter del id de la la solicitud de financiacion
     * @return String que contiene el id de la solicitud de financiacion
     */
    public String getPeticion() {
        return peticion;
    }

    /**
     * Getter del importe concedido de un proyecto
     * @return double que contiene el importe que se ha concedido a un proyecto
     */
    public double getImporteConcedido() {
        return importeConcedido;
    }
}
