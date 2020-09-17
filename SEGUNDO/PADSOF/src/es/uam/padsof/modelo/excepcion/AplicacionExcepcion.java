package es.uam.padsof.modelo.excepcion;

/**
 * Excepcion AplicacionExcepcion que hereda de la clase Exception
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class AplicacionExcepcion extends Exception {
    /**
     * Constructor de la clase AplicacionExcepcion
     * @param s String que contiene el mensaje que define la excepcion
     */
    public AplicacionExcepcion(String s){
        super(s);
    }
}
