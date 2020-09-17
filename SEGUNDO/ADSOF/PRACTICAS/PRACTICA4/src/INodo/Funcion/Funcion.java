package INodo.Funcion;

import INodo.INodo;

import java.util.ArrayList;
import java.util.List;

/**
 * Clase abstracta que implementa la interfaz INodo
 * @author Tomas HIguera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public abstract class Funcion implements INodo {
    private String operador;
    private List <INodo> nodos;
    private int hijos;
    private int etiqueta = -1;

    /**
     * Constructor de la clase Funcion
     * @param operador Operador con el que se setteara la funcio
     * @param hijos Numero de hijos de la funcion
     */
    public Funcion(String operador, int hijos) {
        this.operador = operador;
        this.nodos = new ArrayList<>();
        this.hijos = hijos;
    }

    /**
     * Getter del operador de la funcion
     * @return String que contiene el operador de la funcion
     */
    public String getRaiz(){
        return operador;
    }

    /**
     * Devuelve una lista que contiene los hijos de la funcion
     * @return ArrayList que contiene los INodos descendientes de la funcion
     */
    public List<INodo> getDescendientes(){
        return nodos;
    }

    /**
     * Metodo que incluye un nodo en el arrayList de hijos de la funcion
     * @param nodo Nodo que sera aniadido
     */
    public void incluirDescendiente(INodo nodo){
        nodos.add(nodo.copy());
    }

    /**
     * Metodo toString de la funcion
     * @return String que contiene toda la informacion de la funcion
     */
    @Override
    public String toString() {
        return "(" + getRaiz() + " " + getDescendientes() + ")";
    }

    /**
     * Getter de la etiqueta de una funcion
     * @return Etiqueta de la funcion
     */
    public int getEtiqueta() {
        return etiqueta;
    }

    /**
     * Getter del numero de nodos de la funcion
     * @return Devuelve el numero de nodos hijos de la funcion
     */
    public int getNnodos(){
        int nNodos;

        nNodos = getDescendientes().size();

        for(INodo aux : getDescendientes()){
            nNodos += aux.getNnodos();
        }
        return nNodos;
    }

    /**
     * Setter de la etiqueta del nodo
     * @param etiqueta Etiqueta con la que se setteara el nodo
     */
    public void setEtiqueta(int etiqueta){
        this.etiqueta = etiqueta;
    }

    /**
     * Setter del numero de hijos de la funcion
     * @param hijos Numero de hijos con el que se setteara la funcion
     */
    public void setNHijos(int hijos){
        this.hijos = hijos;
    }

    /**
     * Getter del numero de hijos de la funcion
     * @return Numero de hijos de la funcion
     */
    public int getNHijos(){
        return hijos;
    }

    /**
     * Setter del arrayList de nodos hijo de la funcion
     * @param nodos ArrayList que contiene los nodos con los que se settearan los hijos de la funcion
     */
    public void setNodos(List<INodo> nodos){
        this.nodos = nodos;
    }

    /**
     * Setter del operador de la funcion
     * @param expresion String que contiene el operador de la funcion
     */
    public void setExpresion(String expresion){
        this.operador = expresion;
    }


}
