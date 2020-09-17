package INodo.Terminal;

import INodo.INodo;

import java.util.ArrayList;
import java.util.List;

/**
 * Clase publica TerminalAritmetico que hereda de Terminal
 * @author Tomas HIguera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public class TerminalAritmetico extends Terminal {

    /**
     * Metodo que inicializa el valor del operador del TerminalAritmetico
     * @param operador String que contiene el operador del terminal
     */
    public TerminalAritmetico(String operador) {
        super(operador);
    }

    /**
     * Metodo que devuelve los hijos del Terminal Aritmetico
     * @return ArrayList que contiene los descendientes del terminal
     */
    @Override
    public List<INodo> getDescendientes() {
        return new ArrayList<INodo>();
    }

    /**
     * Metodo que introduce un hijo en el TerminalAritmetico
     * @param nodo Nodo que sera aniadido
     */
    @Override
    public void incluirDescendiente(INodo nodo) { }

    /**
     * Metodo que calcula el valor de la expresion del TerminalAritmetico (Como no tiene hijos, ni es una funcion, devuelve su valor)
     * @return Valor del terminal
     */
    @Override
    public double calcular() {
        return valor;
    }

    /**
     * Metodo que devuelve una copia del TerminalAritmetico
     * @return Copia del terminal
     */
    @Override
    public INodo copy() {
        INodo copy =  new TerminalAritmetico(getRaiz());
        copy.setEtiqueta(getEtiqueta());
        return copy;
    }

    /**
     * Metodo para imprimer los atributos de un terminal
     * @return Strinf que contiene toda la informacion de un terminal
     */
    @Override
    public String toString(){
        return " " + getRaiz();
    }
}
