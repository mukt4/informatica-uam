package INodo.Funcion;

import INodo.INodo;

/**
 * Clase publica FuncionMultiplicacion que hereda de la clase abstracta Funcion
 * @author TOmas HIguera Viso y Guilllermo HOyo Bravo
 * @version 1.0
 */
public class FuncionMultiplicacion extends Funcion{

    /**
     * Constructor de la c,ase FuncionMultiplicacion
     * @param operador String que contieene el operador de la funcion
     * @param hijos Numero de hijos de la funcion
     */
    public FuncionMultiplicacion(String operador, int hijos) {
        super(operador, hijos);
    }

    /**
     * Metodo que calcula el valor de la funcion
     * @return Devuelve el valor de la funcion
     */
    public double calcular(){
        double calculo = 1;

        for(INodo descendiente : getDescendientes()) {
            calculo *= descendiente.calcular();
        }

        return calculo;
    }

    /**
     * Metodo que realiza una copia del objeto y de sus descendientes
     * @return Copia de la funcion
     */
    public INodo copy(){
        INodo copia =  new FuncionMultiplicacion(getRaiz(), getNHijos());
        copia.setEtiqueta(getEtiqueta());
        for (INodo aux : getDescendientes()){
            copia.incluirDescendiente(aux.copy());
        }
        return copia;
    }

    /**
     * Metodo toString de la clase FuncionMultiplicacion
     * @return String que contiene toda la informacion de la funcion
     */
    @Override
    public String toString(){
        StringBuilder retorno = new StringBuilder(" ( *");
        for(INodo descendiente : getDescendientes()){
            retorno.append(descendiente);
        }
        retorno.append(" )");

        return String.valueOf(retorno);
    }

}
