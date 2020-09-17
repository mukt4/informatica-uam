package es.uam.padsof.modelo.excepcion;


/**
 * Excepcion ErrorFichero que hereda de la clase Exception
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class ErrorFichero extends Exception {
    /**
     * Constructor de la clase ErrorFichero
     * @param s String que contiene el mensaje que define la excepcion
     */
    public ErrorFichero(String s){
        super(s);
    }
}
