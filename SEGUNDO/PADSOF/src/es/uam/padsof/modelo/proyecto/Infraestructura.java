package es.uam.padsof.modelo.proyecto;

import es.uam.eps.sadp.grants.GrantRequest;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.time.LocalDate;
import java.io.File;
import java.util.ArrayList;


/**
 * Clase Infraestuctura que extiende clase Proyecto, contiene toda
 * la informacion acerca de un proyecto de tipo Infraestructura
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class Infraestructura extends Proyecto{
    /**
     * ArrayList que contiene los distritos del proyecto
     */
    private ArrayList<Distrito> distritos;

    /**
     * File que contiene el esquema grafico del proyecto
     */
    private File esquemaGrafico;

    /**
     * Constructor de la clase Infraestructura
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param creador UsuarioRegistrado creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param distritos ArrayList que contiene los distritos del proyecto
     * @param esquemaGrafico File que contiene esquema grafico del proyecto
     */
    public Infraestructura(String titulo, String descripcion, double importe, UsuarioRegistrado creador, LocalDate fecha, ArrayList<Distrito> distritos, File esquemaGrafico){
        super(titulo, descripcion, importe, creador, fecha);
        this.distritos = distritos;
        this.esquemaGrafico = esquemaGrafico;
    }

    /**
     * Constructor de la clase Infraestructura
     * @param titulo String que contiene el nombre del proyecto
     * @param descripcion String que contiene la descripcion del proyecto
     * @param importe Importe solicitado en el proyecto
     * @param creador Colectivo creador del proyecto
     * @param fecha LocalDate que contiene la fecha en la que se crea el proyecto
     * @param distritos ArrayList que contiene los distritos del proyecto
     * @param esquemaGrafico File que contiene esquema grafico del proyecto
     */
    public Infraestructura(String titulo, String descripcion, double importe, Colectivo creador, LocalDate fecha, ArrayList<Distrito> distritos, File esquemaGrafico){
        super(titulo, descripcion, importe, creador, fecha);
        this.distritos = distritos;
        this.esquemaGrafico = esquemaGrafico;
    }

    // Getters y setters

    /**
     * Getter de los distritos del proyecto
     * @return ArrayList que contiene los distritos del proyecto
     */
    public ArrayList<Distrito> getDistritos() {
        return distritos;
    }

    /**
     * Getter del esquema grafico del proyecto
     * @return File que contiene el esquema grafico del proyecto
     */
    public File getEsquemaGrafico() {
        return esquemaGrafico;
    }

    // Metodos de GrantRequest

    /**
     * Metodo que devuelve informacion acerca del proyecto
     * Metodo implementado de la interfaz GrantRequest
     * @return String que contiene informacion acerca del proyecto
     */
    public String getExtraData(){
        return "Esquema grafico: '" + esquemaGrafico.getName() + "' + distritos: " + distritos;
    }

    /**
     * Metodo que devuelve el tipo de proyecto
     * Metodo implementado de la interfaz GrantRequest
     * @return Objeto de tipo GrantRequest.ProjectKind que contiene el tipo de proyecto
     */
    public GrantRequest.ProjectKind getProjectKind(){
        return GrantRequest.ProjectKind.valueOf("Infrastructure");
    }
}
