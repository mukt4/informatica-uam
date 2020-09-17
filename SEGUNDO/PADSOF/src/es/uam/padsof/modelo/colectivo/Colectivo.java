package es.uam.padsof.modelo.colectivo;

import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;


/**
 * Clase Colectivo que contiene toda la informacion sobre un colectivo
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class Colectivo implements Observer, Serializable{
    /**
     * Serial Version para serializar la aplicacion
     */
    private static final long serialVersionUID = 1L;

    /**
     * Representante del colectivo de tipo UsuarioRegistrado
     */
    private UsuarioRegistrado representante;

    /**
     * ArrayList que contiene los participantes del colectivo
     */
    private ArrayList<UsuarioRegistrado> participantes;

    /**
     * Nombre del colectivo
     */
    private String nombre;

    /**
     * Colectivo hijo del Colectivo
     */
    private ArrayList<Colectivo> colectivosHijo;

    /**
     * Constructor de la clase Colectivo
     * @param nombre String que contien el nombre del colectivo que debe de ser unico en la aplicacions
     * @param representante UsuarioRegistrado que crea el proyecto
     */
    public Colectivo(String nombre, UsuarioRegistrado representante){
        this.nombre = nombre;
        this.representante = representante;
        this.participantes = new ArrayList<>();
        this.colectivosHijo = new ArrayList<>();
    }

    /**
     * Metodo para crear colectivo hijo dentro del colectivo
     * @param nombre String que contiene el nombre del colectivo hijo
     * @return boolean: true si se ha podido crear colectivo hijo y false si no
     */
    public boolean crearColectivoHijo(String nombre){
        if(!comprobarNombre(nombre)) {
            colectivosHijo.add(new Colectivo(nombre, representante));
            return true;
        }
        return false;
    }

    /**
     * Metodo para seguir al colectivo siendo un usuario registrado
     * @param usuario UsuarioRegistrado que va a seguir al colectivo
     * @return boolean: true si se ha podido seguir al colectivo o false si ha habido algun error
     */
    public boolean seguirColectivo(UsuarioRegistrado usuario){
        if(comprobarUsuario(usuario))
            return false;

        participantes.add(usuario);

        return true;
    }

    /**
     * Metodo para abandonar colectivo siendo un usuario registrado
     * @param usuario UsuarioRegistrado que va a abandonar el colectivo
     * @return boolean: true si se ha podido seguir al colectivo o false si ha habido algun error
     */
    public boolean abandonarColectivo(UsuarioRegistrado usuario){
        for(UsuarioRegistrado u : participantes){
            if(u.getNombreUsuario().equals(usuario.getNombreUsuario())){
                participantes.remove(u);
                return true;
            }
        }
        return false;
    }

    /**
     * Metodo que comprueba si un colectivo o alguno de sus hijos tiene un determinado nombre
     * @param nombre String que contiene el nombre que se va a comprobar
     * @return boolean: true si el nombre esta contenido o false si no lo esta
     */
    public boolean comprobarNombre(String nombre) {
        if(this.nombre.equals(nombre))
            return true;

        for(Colectivo c : colectivosHijo) {
            if(c.comprobarNombre(nombre))
                return true;
        }

        return false;
    }

    /**
     * Metodo que comprueba si un usuario es el creador del colectivo
     * @param nombre String que contiene el nombre del usuario que se va a comprobar
     * @return boolean: true si es el creador y false si no lo es
     */
    public boolean comprobarCreador(String nombre){
        return representante.getNombreUsuario().equals(nombre);
    }

    /**
     * Metodo que comprueba si un usuario sigue un colectivo
     * @param usuario UsuarioRegistrado que se desea comprobar
     * @return boolean: true si el usuario sigue al colectivo o false si no
     */
    public boolean comprobarUsuario(UsuarioRegistrado usuario){
        for(UsuarioRegistrado u : participantes){
            if(u.getNombreUsuario().equals(usuario.getNombreUsuario()))
                return true;
        }

        return false;
    }

    /**
     * Metodo que actualiza las notificaciones de los seguidores del colectivo
     * Metodo implementado de la interfaz observer
     * @param observable Clase que esta siendo observada
     * @param o Object que contiene la informacion que se escribira a los seguidores
     */
    @Override
    public void update(Observable observable, Object o) {
        String notificacion = "Notificacion colectivo '" + nombre + "': " + o;
        for(UsuarioRegistrado u : participantes)
            u.update(observable, notificacion);
    }

    /**
     * Metodo toString para representar la clase
     * @return String que contiene toda la informacion acerca del colectivo
     */
    @Override
    public String toString(){
        return nombre + "\t" + "Participantes: " + participantes + "\t" + "\t" + "Colectivo hijo: " + colectivosHijo;
    }

    /**
     * Metodo que comprieba si dos colectivos son iguales
     * @param colectivo Colectivo con el que se realizara la comprobacion
     * @return boolean: true si son iguales o false si no
     */
    public boolean equals(Colectivo colectivo){
        return this.getNombre().equals(colectivo.getNombre());
    }

    // Getters y setters

    /**
     * Getter del atributo nombre de la clase
     * @return String que contiene el nombre del colectivo
     */
    public String getNombre() {
        return nombre;
    }

    /**
     * Getter del atributo participantes de la clase
     * @return ArrayList que contiene los participantes del colectivo
     */
    public ArrayList<UsuarioRegistrado> getParticipantes() {
        return participantes;
    }

    /**
     * Getter del atributo representante de la clase
     * @return UsuarioRegistrado que contiene el representante del colectivo
     */
    public UsuarioRegistrado getRepresentante(){
        return representante;
    }

    /**
     * Getter del atributo colectivosHijo de la clase
     * @return ArrayList que contiene todos los colectivos hijo del colectivo
     */
    public ArrayList<Colectivo> getColectivosHijo(){
        return colectivosHijo;
    }
}
