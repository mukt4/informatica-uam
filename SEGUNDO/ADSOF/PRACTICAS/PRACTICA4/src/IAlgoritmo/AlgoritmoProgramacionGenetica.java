package IAlgoritmo;

import Excepciones.ArgsDistintosFuncionesException;
import Excepciones.CruceNuloException;
import IDominio.DominioAritmetico;
import IDominio.IDominio;
import IIndividuo.IIndividuo;
import INodo.Funcion.Funcion;
import INodo.INodo;
import INodo.Terminal.Terminal;
import IIndividuo.Individuo;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.*;

/**
 * Clase publica AlgoritmoProgramacionGenetica que implementa la interfaz IAlgoritmo
 * @author Tomas HIguera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public class AlgoritmoProgramacionGenetica implements IAlgoritmo {
    private List<Terminal> terminales;
    private List<Funcion> funciones;
    private List<IIndividuo> individuos;
    private int profundidad;
    private int nIndividuos;
    private double probabilidad;
    private int nGeneraciones;
    private int k;
    private IDominio dominio;

    /**
     * Metodo para inicializar el conjunto de terminales
     * @param terminales Lista que contiene las terminales con las que se incializara el dominio
     */
    public void defineConjuntoTerminales(List<Terminal> terminales){
        this.terminales = terminales;
    }

    /**
     * Metodo para inicializar eñ conjunto de funciones
     * @param funciones Lista que contiene las funciones con las que se inicializara el dominio
     */
    public void defineConjuntoFunciones(List<Funcion> funciones){
        this.funciones = funciones;
    }

    /**
     * Metodo para crear la poblacion de Individuos inicial aleatoria
     */
    public void crearPoblacion(){
        IIndividuo indi = new Individuo();
        individuos = new ArrayList<>();
        for(int i = 0; i < nIndividuos; i++){
            indi.crearIndividuoAleatorio(profundidad, terminales, funciones);
            individuos.add(indi);
        }
    }

    /**
     * Metodo que realiza un cruce aleatorio entre 2 individuos
     * @param prog1 Uno de los individuos que se cruzaran
     * @param prog2 El otro individuo que se cruzara
     * @return La lista que contiene dos individuos que han sido el resultado del cruce
     * @throws CruceNuloException Esta excepcion se lanza cuando se produce un error en el cruce
     */
    public List<IIndividuo> cruce(IIndividuo prog1, IIndividuo prog2) throws CruceNuloException{
        Random rand = new Random();
        INodo copy1;
        INodo copy2;
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

    /**
     * Metodo para crear una nueva generacion segun la primera generacion creada
     * Este metodo tomara un porcentaje de individuos de los cuales, se ecogeran unos cuantos deforma aleatoria
     * los 2 con mejor fitness se cruzaran y pasaran de generacion.
     * El resto del porcentage de la poblacion para los que  no fueron seleccionados para torneo, pasaran directamente de generacion.
     */
    public void crearNuevaPoblacion(){
        List<IIndividuo> generacion = new ArrayList<>();
        List<IIndividuo> cruzados = new ArrayList<>();

        Random rand = new Random();
        int random = 0;
        int maximo = (int) ((probabilidad * individuos.size())/100);
        double fitnessMax = -1;
        double fitnessMax2 = -1;
        int[] posicion = {-1,-1};

        random = rand.nextInt(maximo);

        for(int i = 0; i < individuos.size(); i++){
            generacion.add(individuos.get(maximo + i));
        }

        for(int i = 0; i < k; i++){
            if(fitnessMax == -1 && fitnessMax2 == -1){
                fitnessMax = dominio.calcularFitness(individuos.get(random));
                posicion[0] = random;
            }
            else{
                if(fitnessMax < dominio.calcularFitness(individuos.get(random))){
                    if(fitnessMax2 <= fitnessMax){
                        fitnessMax2 = fitnessMax;
                        posicion[1] = posicion[0];

                        fitnessMax = dominio.calcularFitness(individuos.get(random));
                        posicion[0] = random;
                    }
                }
            }
        }

        try{
            cruzados = this.cruce(individuos.get(posicion[0]), individuos.get(posicion[1]));
        }catch (CruceNuloException e) {
            e.printStackTrace();
        }
        generacion.addAll(cruzados);

        individuos = generacion;
    }

    /**
     * Metodo que ejecuta el algoritmo de programacion genetica especificado con un determinado dominio
     * @param dominio Dominio con el que se ejecutara el algoritmo
     */
    public void ejecutar(IDominio dominio){
        int aux[] = {2,2,2,2,2,2,2,2,2};
        String aux2[] = {"x","x","x","x","x","x","+","+","+"};
        int contador = 0;
        Scanner sc = new Scanner(System.in);
        defineConjuntoTerminales(dominio.definirConjuntoTerminales("x","x","x","x","x","x","x","x","x","x"));
        this.dominio = dominio;
        try{
            defineConjuntoFunciones(dominio.definirConjuntoFunciones(aux , aux2));
        }catch (ArgsDistintosFuncionesException e){
            e.printStackTrace();
        }

        System.out.println("Introduzca la profundidad de los individuos: ");
        profundidad = sc.nextInt();
        System.out.println("Introduzca el numero de individuos del algoritmo: ");
        nIndividuos = sc.nextInt();
        System.out.println("Introduzca la probabilidad: ");
        probabilidad = sc.nextDouble();
        System.out.println("Introduzca el numero de generaciones: ");
        nGeneraciones = sc.nextInt();
        System.out.println("Introduzca la k para el torneo");
        k = sc.nextInt();
        crearPoblacion();

        crearPoblacion();
        while(contador != nGeneraciones){
            for(int i = 0; i < individuos.size(); i++){
                if(dominio.calcularFitness(individuos.get(i)) == 21){
                    System.out.println("Individuo genereda con fitness perfecto");
                    individuos.get(i).writeIndividuo();
                    return;
                }
                else{
                    System.out.println("Individuo" + i);
                    individuos.get(i).writeIndividuo();
                }
            }
            contador++;
        }

    }

}
