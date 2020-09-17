package es.uam.padsof.modelo.excepcion;


/**
 * Excepcion ColectivoExcepcion que hereda de la clase Exception
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class ColectivoExcepcion extends Exception{
    /**
     * Constructor de la clase ColectivoExcepcion
     * @param s String que contiene el mensaje que define la excepcion
     */
    public ColectivoExcepcion(String s){
        super(s);
    }
}
