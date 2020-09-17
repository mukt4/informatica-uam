package INodo.Funcion;

import INodo.INodo;

/**
 * Clase publica FuncionSuma que hereda de la clase Funcion
 * @author Tomas HIguera Viso y Guillermo HOyo Bravo
 * @version 1.0
 */
public class FuncionSuma extends Funcion {

    /**
     * Constructor de la clase FuncionSUma
     * @param operador String que contiene el operador de la funcion
     * @param hijos Numero de hijos de la funcion
     */
    public FuncionSuma(String operador, int hijos) {
        super(operador, hijos);
    }

    /**
     * Metodo que calcula el valor de la funcion
     * @return Double que contiene el valor de la funcion
     */
    public double calcular(){
        double calculo = 0;

        for(INodo descendiente : getDescendientes()){
            calculo += descendiente.calcular();
        }

        return calculo;
    }

    /**
     * Metodo que realiza una copia de la funcion y de sus descendientes
     * @return Nodo que contiene una copia de la funcion
     */
    public INodo copy(){
        INodo copia =  new FuncionSuma(getRaiz(), getNHijos());
        copia.setEtiqueta(getEtiqueta());
        for (INodo aux : getDescendientes()){
            copia.incluirDescendiente(aux.copy());
        }
        return copia;
    }

    /**
     * Metodo toString de la clase FuncionSuma
     * @return String que contiene toda la informacion de la funcion
     */
    @Override
    public String toString(){
        StringBuilder retorno = new StringBuilder(" ( +");
        for(INodo descendiente : getDescendientes()){
            retorno.append(descendiente);
        }
        retorno.append(" )");

        return String.valueOf(retorno);
    }
}

