package es.uam.padsof.modelo.excepcion;


/**
 * Excepcion ProyectoExcepcion que hereda de la clase Exception
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class ProyectoExcepcion extends Exception{
    /**
     * Constructor de la clase ProyectoExcepcion
     * @param s String que contiene el mensaje que define la excepcion
     */
    public ProyectoExcepcion(String s){
        super(s);
    }
}
