package es.uam.padsof.modelo.usuario.registrado;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

/**
 * Clase UsuarioRegistrado que contiene toda la informacion acerca
 * de un usuario registrado
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class UsuarioRegistrado implements Observer, Serializable{
    /**
     * Serial Version para serializar la aplicacion
     */
    private static final long serialVersionUID = 1L;

    /**
     * DNI del usuario registrado
     */
    private String dni;

    /**
     * Contrasena del usuario registrado
     */
    private String contrasena;

    /**
     * Nombre del usuario registrado
     */
    private String nombreUsuario;

    /**
     * EstadoUsuario del usuario en la aplicacion
     */
    private EstadoUsuario estado;

    /**
     * ArrayList que contiene las notificaciones del usuario
     */
    private ArrayList<String> notificaciones;

    /**
     * Constructor de la clase UsuarioRegistrado
     * @param dni String que contiene el dni del usuario
     * @param contrasena String que contiene la contrasena del usuario
     * @param nombreUsuario String que contiene el nombre del usuario
     */
    public UsuarioRegistrado(String dni, String contrasena, String nombreUsuario){
        this.nombreUsuario = nombreUsuario;
        this.contrasena = contrasena;
        this.dni = dni;
        this.estado = EstadoUsuario.ACEPTADO;
        this.notificaciones = new ArrayList<>();
    }

    /**
     * Metodo que cambia el estado de un usuario a bloqueado
     */
    public void bloqueado(){
        estado = EstadoUsuario.BLOQUEADO;
    }

    /**
     * Metodo que cambia el estado de un usuario a aceptado
     */
    public void aceptado(){
        estado = EstadoUsuario.ACEPTADO;
    }

    /**
     * Metodo que comprueba si el estado de un usuario es BLOQUEADO
     * @return boolean: true si cumple los requisitos o false si no
     */
    public boolean isBloqueado(){
        return estado == EstadoUsuario.BLOQUEADO;
    }

    /**
     * Metodo que limpia el ArrayList de notificaciones del usuario
     */
    public void limpiarNotificaciones(){
        notificaciones = new ArrayList<>();
    }

    /**
     * Metodo que actualiza las notificaciones del usuario
     * Metodo implementado de la interfaz observer
     * @param observable Clase que esta siendo observada
     * @param o Object que contiene la informacion que se escribira al usuario
     */
    @Override
    public void update(Observable observable, Object o) {
        this.notificaciones.add((String) o);
    }

    /**
     * Metodo toString para representar la clase UsuarioRegistrado
     * @return String que contiene toda la informacion acerca del usuario
     */
    @Override
    public String toString() {
        return dni + "\t" + nombreUsuario + "\tEstado: " + estado;
    }

    // Getters y setters

    /**
     * Getter del atributo dni de la clase
     * @return String que contiene el dni del usuario
     */
    public String getDni() {
        return dni;
    }

    /**
     * Getter del atributo nombre de la clase
     * @return String que contiene el nombre del usuario
     */
    public String getNombreUsuario() {
        return nombreUsuario;
    }

    /**
     * Getter del atributo contrasena de la clase
     * @return String que contiene la contrasena del usuario
     */
    public String getContrasena() {
        return contrasena;
    }

    /**
     * Getter del atributo notificaciones de la clase
     * @return ArrayList que contiene las notificaciones del usuario
     */
    public ArrayList<String>  getNotificaciones(){
        return notificaciones;
    }

    /**
     * Setter del atributo notificaciones de la clase
     * @param notificaciones ArrayList que contiene las notificaciones que se van a settear
     */
    public void setNotificaciones(ArrayList<String> notificaciones){
        this.notificaciones = notificaciones;
    }

    /**
     * Metodo que comprueba sin usuario es igual que otro comprobando nombres de usuario o dni
     * @param usuarioRegistrado UsuarioRegistrado con el que se hara la comprobacion
     * @return boolean: true si son iguales o false si no
     */
    public boolean equals(UsuarioRegistrado usuarioRegistrado) {
        return usuarioRegistrado.getNombreUsuario().equals(nombreUsuario) || usuarioRegistrado.getDni().equals(dni);
    }
}
