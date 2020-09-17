package Cruce;

import Excepciones.CruceNuloException;
import IIndividuo.IIndividuo;
import INodo.INodo;
import IIndividuo.Individuo;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Clase publica para probar el metodo PruebaCruce
 * @author Tomas HIguera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public class PruebaCruce {
    /**
     * Metodo de creacion de un Metodo que probara la funcionalidad de cruce
     * El cruce consiste en coger los 2 individuos pasados por parametros y seleccionar, de cada uno,
     * uno de sus nodos de forma aleatoria, de modo que ambos nodos se intercambien de Individuos para crear unos nuevos.
     * @param prog1 Uno de los individuos que se cruzaran
     * @param prog2 El otro individuo que se cruzara
     * @return La lista que contiene dos individuos que han sido el resultado del cruce
     * @throws CruceNuloException Esta excepcion se lanza cuando se produce un error en el cruce
     */
    public List<IIndividuo> cruce(IIndividuo prog1, IIndividuo prog2) throws CruceNuloException {
        Random rand = new Random();
        INodo copy1;
        INodo copy2;
        INodo aux;
        IIndividuo indiv1 = new Individuo();
        IIndividuo indiv2 = new Individuo();

        indiv1.setExpresion(prog1.getExpresion().copy());
        indiv2.setExpresion(prog2.getExpresion().copy());

        ArrayList<IIndividuo> lista = new ArrayList<>();
        int n1 = rand.nextInt(prog1.getNumeroNodos()) + 1;
        int n2 = rand.nextInt(prog2.getNumeroNodos()) + 1;

        if(n1 == 1 && n2 == 1)
            throw new CruceNuloException();

        copy1 = prog1.getNodoEtiqueta(n1).copy();
        copy2 = prog2.getNodoEtiqueta(n2).copy();

        if(prog1.getPadreNodoEtiqueta(n1).getEtiqueta() == n1){
            indiv1.setExpresion(copy2);
        }
        else{
            for(int i = 0; i < prog1.getPadreNodoEtiqueta(n1).getDescendientes().size(); i++){
                if(prog1.getPadreNodoEtiqueta(n1).getDescendientes().get(i).getEtiqueta() == n1){
                    indiv1.getPadreNodoEtiqueta(n1).getDescendientes().set(i, copy2);
                }
            }
        }

        if(prog2.getPadreNodoEtiqueta(n2).getEtiqueta() == n2){
            indiv2.setExpresion(copy1);
        }
        else{
            for(int i = 0; i < prog2.getPadreNodoEtiqueta(n2).getDescendientes().size(); i++){
                if(prog2.getPadreNodoEtiqueta(n2).getDescendientes().get(i).getEtiqueta() == n2){
                    indiv2.getPadreNodoEtiqueta(n2).getDescendientes().set(i, copy1);
                }
            }
        }
        lista.add(indiv1);
        lista.add(indiv2);

        return lista;
    }
}