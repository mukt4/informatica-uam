package es.uam.padsof.modelo.excepcion;

/**
 * Excepcion UsuarioExcepcion que hereda de la clase Exception
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class UsuarioExcepcion extends Exception {
    /**
     * Constructor de la clase UsuarioExcepcion
     * @param s String que contiene el mensaje que define la excepcion
     */
    public UsuarioExcepcion(String s) {
        super(s);
    }
}
