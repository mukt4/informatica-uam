package INodo.Funcion;

import INodo.INodo;

/**
 * Clase publica FuncionResta que hereda de la clase Funcion
 * @author Tomas HIguera Viso y Guillermo HOyo Bravo
 * @version 1.0
 */
public class FuncionResta extends Funcion {

    /**
     * Constructor de la clase FuncionResta
     * @param operador String que contiene el operadir de la funcion
     * @param hijos Numero de hijos de la funcion
     */
    public FuncionResta(String operador, int hijos) {
        super(operador, hijos);
    }

    /**
     * Metodo que calcula el valor de la funcion
     * @return Double que contiene el valor de la funcion
     */
    public double calcular(){
        double calculo;
        int i;

        calculo = getDescendientes().get(0).calcular();

        for(i = 1; i < getDescendientes().size(); i++){
            calculo = calculo - getDescendientes().get(i).calcular();
        }
        return calculo;
    }

    /**
     * Metodo que realiza una copia de la funcion y de sus descendientes
     * @return Copia de la funcion
     */
    public INodo copy(){
        INodo copia =  new FuncionResta(getRaiz(), getNHijos());
        copia.setEtiqueta(getEtiqueta());
        for (INodo aux : getDescendientes()){
            copia.incluirDescendiente(aux.copy());
        }
        return copia;
    }

    /**
     * Metodo toString de la clase FuncionResta
     * @return String que contiene toda la informacion de la funcion
     */
    @Override
    public String toString(){
        StringBuilder retorno = new StringBuilder(" ( -");
        for(INodo descendiente : getDescendientes()){
            retorno.append(descendiente);
        }
        retorno.append(" )");

        return String.valueOf(retorno);    }
}
