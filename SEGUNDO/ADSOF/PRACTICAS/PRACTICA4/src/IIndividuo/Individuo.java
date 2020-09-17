package IIndividuo;

import Excepciones.CruceNuloException;
import INodo.Funcion.Funcion;
import INodo.INodo;
import INodo.Terminal.Terminal;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Clase publica individuo que implemta la interfaz IIndividuo
 * @author Tomas Higuera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public class Individuo implements IIndividuo {
    private INodo nodos;
    private double fitness;
    private int etiqueta = 1;

    public Individuo() {
    }

    /**
     * Metodo que devuelve el atributo INodo nodo del Individuo.
     * @return Devuelve el nodo raiz del individuo
     */
    @Override
    public INodo getExpresion() {
        return nodos;
    }

    /**
     * Metodo que establece el valor del atributo INodo del nodo del Individuo
     * @param expresion Setea una copia del nodo raiz del individuo
     */
    @Override
    public void setExpresion(INodo expresion) {
        nodos = expresion.copy();
    }

    /**
     * Metodo que devuelve el atributo de fitness del Individuo
     * @return Devuelve el fitness del individuo
     */
    @Override
    public double getFitness() {
        return fitness;
    }

    /**
     * Metodo que estable el valor del atributo de fitness del Individuo
     * @param fitness Fitness con el que se seteara el individuo
     */
    @Override
    public void setFitness(double fitness) {
        this.fitness = fitness;
    }

    /**
     * Metodo recursivo usado para poder crear un Individuo con cualquier profundidad en la función crearIndividuoAleatorio
     * @param nodo Nodo raiz del individuo
     * @param terminals Lista de terminales con las que se creara el individuo aleatorio
     * @param funcions Lista de funciones con la que se creara el individuo aleatorio
     * @param fA Lista de funciones
     * @param fI Lista de terminales
     * @param profundidad Profundidad maxima del individuo
     * @param z Valor k
     */
    public void crearIndividuoAlearioRec(INodo nodo, List<Terminal> terminals, List<Funcion> funcions, List<Funcion> fA, List<Funcion> fI, int profundidad, int z) {
        int aleatorio = 0;
        Random rand = new Random();

        aleatorio = rand.nextInt(funcions.size());

        if(nodo.getDescendientes().size() == 0){
            profundidad--;
            nodo.incluirDescendiente(funcions.get(aleatorio));
            fA.add(funcions.get(aleatorio));
            fI.add(funcions.get(aleatorio));
            funcions.remove(aleatorio);
        }

        else if (nodo.getDescendientes().size() != nodo.getNHijos()) {
            for (INodo hijo1 : nodo.getDescendientes()) {
                if (hijo1.getDescendientes().size() == hijo1.getNHijos()) {
                    fI.remove(hijo1);
                } else {
                    nodo.incluirDescendiente(funcions.get(aleatorio));
                    fA.add(funcions.get(aleatorio));
                    fI.add(funcions.get(aleatorio));
                    fI.remove(z);
                    funcions.remove(aleatorio);
                    break;
                }
            }
        } else {
            fI.remove(nodo);
            for (INodo hijo1 : nodo.getDescendientes()) {
                crearIndividuoAlearioRec(hijo1, terminals, funcions, fA, fI, profundidad, z);
            }
        }
    }

    /**
     * Metodo de creación de un Individuo aleatorio de forma recursiva según los argumentos pasados a la función
     * @param profundidad Profundidad maxima del individuo
     * @param terminales Lista de terminales con las que se creara el individuo
     * @param funciones Lista de funciones con las que se creara el individuo
     */
    @Override
    public void crearIndividuoAleatorio(int profundidad, List<Terminal> terminales, List<Funcion> funciones) {
        int z = 0;
        int aleatorio = 0;
        List<Funcion> fAsignadas = new ArrayList<>();
        List<Funcion> fIncompletas = new ArrayList<>();

        List<Funcion> copia = new ArrayList<>(funciones);

        Random rand = new Random();

        aleatorio = rand.nextInt(copia.size());
        profundidad--;
        this.setExpresion(copia.get(aleatorio));

        fAsignadas.add((Funcion) copia.get(aleatorio).copy());
        fIncompletas.add((Funcion) copia.get(aleatorio).copy());

        copia.remove(aleatorio);

        while (copia.size() != 0) {
            crearIndividuoAlearioRec(this.nodos, terminales, copia, fAsignadas, fIncompletas, profundidad, z);
        }

        while(fIncompletas.size() != 0){
            for (INodo nodo : this.nodos.getDescendientes()) {
                if (nodo.getDescendientes().size() == nodo.getNHijos()) {
                    fIncompletas.remove(z);
                }
                while(nodo.getDescendientes().size() != nodo.getNHijos()){
                    aleatorio = rand.nextInt(terminales.size());
                    nodo.incluirDescendiente(terminales.get(aleatorio));
                }
            }
        }

    }

    /**
     * Metodo para calcular el valor de la expresion del Individuo
     * @return Devuelve el valor de la expresion que define el individuo
     */
    @Override
    public double calcularExpresion() {
        return nodos.calcular();
    }

    /**
     * Metodo que devuelve el numero de nodos que tiene el Individuo
     * @return Devuelve el numero de nodos del individuo
     */
    @Override
    public int getNumeroNodos() {
        return nodos.getNnodos();
    }

    /**
     * Metodo para imprimir por pantalla el Individuo entero
     */
    @Override
    public void writeIndividuo() {
        System.out.print("Expresion:" + nodos);
    }

    /**
     * Etiqueta los nodos del individuo
     */
    public void etiquetaNodos() {
        INodo aux = getExpresion();
        aux.setEtiqueta(etiqueta);
        etiqueta++;
        for (INodo hijo : aux.getDescendientes()) {
            etiquetarNodo(hijo);
        }
    }

    /**
     * Metodo recursivo para etiquetar nodos
     * @param nodo Nodo que sera etiquetado
     */
    public void etiquetarNodo(INodo nodo) {
        nodo.setEtiqueta(etiqueta);
        etiqueta++;
        for (INodo hijo : nodo.getDescendientes()) {
            etiquetarNodo(hijo);
        }
    }

    /**
     * Funcion que devuelve el padre del nodo con una determinada etiqueta
     * @param etiqueta Etiqueta que se buscara en el individuo
     * @return Padre del nodo con la etiqueta determinada
     * @throws CruceNuloException Esta excepcion se lanzara si no se encuentra el nodo con la etiqueta
     */
    public INodo getPadreNodoEtiqueta(int etiqueta) throws CruceNuloException {
        INodo aux = getExpresion();

        if (aux.getEtiqueta() == etiqueta) {
            return aux;
        }

        for (INodo hijo : aux.getDescendientes()) {
                if(hijo.getEtiqueta() == etiqueta)
                    return aux;
                INodo retorno = getPadreNodoRecursivo(etiqueta, hijo);
                if (retorno != null)
                    return retorno;
        }
        throw new CruceNuloException();
    }

    /**
     * Funcion que busca el padre del nodo con una determinada etiqueta de forma recursiva
     * @param etiqueta Etiqueta que se busca en el individuo
     * @param nodo Nodo por el que se buscara de forma recursiva
     * @return Devuelve el padre del nodo o null si no se encuentra en esa rama
     */
    public INodo getPadreNodoRecursivo(int etiqueta, INodo nodo) {
        INodo aux;

        for (INodo hijo : nodo.getDescendientes()) {
            if(hijo.getEtiqueta() == etiqueta)
                return nodo;
            aux = getPadreNodoRecursivo(etiqueta, hijo);
            if(aux != null)
                return aux;
        }
        return null;
    }

    /**
     * Metodo que busca un nodo con una determinada etiqueta
     * @param etiqueta Etiqueta que se buscara en el individuo
     * @return Devuelve el nodo con una etiqueta determinada
     * @throws CruceNuloException Se lanzara esta excepcion si no se encuentra el nodo
     */
    public INodo getNodoEtiqueta(int etiqueta) throws CruceNuloException {
        INodo aux = getExpresion();

        if (aux.getEtiqueta() == etiqueta) {
            return aux;
        }

        for (INodo hijo : aux.getDescendientes()) {
            INodo retorno = getNodoEtiquetaRecursivo(etiqueta, hijo);
            if (retorno != null)
                return retorno;
        }
        throw new CruceNuloException();
    }

    /**
     * Devuelve un nodo con una determinada etiqueta
     * @param etiqueta Etiqueta que se busca en el individuo
     * @param nodo Nodo por el que se buscara de forma recursiva
     * @return Devuelve el nodo con una etiqueta determinada o null si no se encuentra en esa rama
     */
    public INodo getNodoEtiquetaRecursivo(int etiqueta, INodo nodo){
        INodo aux;

        if(nodo.getEtiqueta() == etiqueta)
            return nodo;

        for (INodo hijo : nodo.getDescendientes()) {
            aux = getNodoEtiquetaRecursivo(etiqueta, hijo);
            if(aux != null)
                return aux;
        }
        return null;
    }
}