package INodo.Terminal;

import INodo.INodo;

import java.util.ArrayList;
import java.util.List;

/**
 * Clase Abstracta Terminal que implementa la interfaz INodo
 * @author Tomas Higuera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public abstract class Terminal implements INodo {
    private String operador;
    private int etiqueta = -1;
    public static double valor;

    /**
     * Inicializa el atributo operador de la clase
     * @param operador String que contiene el operador del terminal
     */
    public Terminal(String operador) {
        this.operador = operador;
    }

    /**
     * Devuelve el valor del atributo operador de la clase
     * @return String que contiene el operador del terminal
     */
    public String getRaiz(){
        return operador;
    }

    /**
     * Imprime los atributos de un Terminal
     * @return String que contiene toda la informacion de un terminal
     */
    @Override
    public String toString() {
        return getRaiz();
    }

    /**
     * Metodo que devuelve la etiqueta de un nodo Terminal
     * @return Int que contiene el valor de la etiqueta de un terminal
     */
    public int getEtiqueta() {
        return etiqueta;
    }

    /**
     * Metodo que devuelve el numero de nodos total del termina, contando sus hijos
     * @return Numero de hijos del terminal(0)
     */
    public int getNnodos(){
        return 0;
    }

    /**
     * Metodo que inicializa los nodos hijos del terminal
     * @param nodos Setter de los hijos del terminal
     */
    public void setNodos(List<INodo> nodos){

    }

    /**
     * Metodo que Inicializa el atributo operador del terminal
     * @param expresion String que contiene el operador del terminal
     */
    public void setExpresion(String expresion){
        this.operador = expresion;
    }

    /**
     * Metodo que inicializa el valor del atributo etiqueta del terminal
     * @param etiqueta Setter del valor de la etiqyeta del terminal
     */
    public void setEtiqueta(int etiqueta){
        this.etiqueta = etiqueta;
    }

    /**
     * Metodo que devueleve el numero de hijos que contiene el Terminal
     * @return Numero de hijos de terminal(0)
     */
    public int getNHijos(){
        return 0;
    }

    /**
     * Metodo que inicializa el valor del Atributo hijos del terminal
     * @param hijos Setter del numero de hijos del terminal
     */
    public void setNHijos(int hijos){

    }
}
