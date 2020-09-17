package es.uam.padsof.modelo.proyecto;

import es.uam.eps.sadp.grants.GrantRequest;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.time.LocalDate;


/**
 * Clase ProyectoSocial que extiende clase Proyecto, contiene toda
 * la informacion acerca de un proyecto de tipo ProyectoSocial
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class ProyectoSocial extends Proyecto{
    /**
     * String que contiene el grupo social al que va dirigido el proyecto
     */
    private String grupoSocial;

    /**
     * Ambito del proyecto
     */
    private Ambito ambito;

    /**
     * Constructor de la clase ProyectoSocial
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param creador UsuarioRegistrado creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param ambito Ambito que contiene el ambito del proyecto
     * @param grupoSocial String que contiene el grupo social al que va dirigido el proyecto
     */
    public ProyectoSocial(String titulo, String descripcion, double importe, UsuarioRegistrado creador, LocalDate fecha, String grupoSocial, Ambito ambito){
        super(titulo, descripcion, importe, creador, fecha);
        this.grupoSocial = grupoSocial;
        this.ambito = ambito;
    }

    /**
     * Constructor de la clase ProyectoSocial
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param creador Colectivo creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param ambito Ambito que contiene el ambito del proyecto
     * @param grupoSocial String que contiene el grupo social al que va dirigido el proyecto
     */
    public ProyectoSocial(String titulo, String descripcion, double importe, Colectivo creador, LocalDate fecha, String grupoSocial, Ambito ambito){
        super(titulo, descripcion, importe, creador, fecha);
        this.grupoSocial = grupoSocial;
        this.ambito = ambito;
    }

    // Getters y setters

    /**
     * Getter del grupo social del proyecto
     * @return String que contiene el grupo social del proyecto
     */
    public String getGrupoSocial() {
        return grupoSocial;
    }

    /**
     * Getter del ambito del proyecto
     * @return Ambito del proyecto
     */
    public Ambito getAmbito() {
        return ambito;
    }

    // Metodos de GrantRequest

    /**
     * Metodo que devuelve informacion acerca del proyecto
     * Metodo implementado de la interfaz GrantRequest
     * @return String que contiene informacion acerca del proyecto
     */
    public String getExtraData(){
        return "Grupo social: '" + grupoSocial + "' + ambito: " + ambito;
    }

    /**
     * Metodo que devuelve el tipo de proyecto
     * Metodo implementado de la interfaz GrantRequest
     * @return Objeto de tipo GrantRequest.ProjectKind que contiene el tipo de proyecto
     */
    public GrantRequest.ProjectKind getProjectKind(){
        return GrantRequest.ProjectKind.valueOf("Social");
    }
}
