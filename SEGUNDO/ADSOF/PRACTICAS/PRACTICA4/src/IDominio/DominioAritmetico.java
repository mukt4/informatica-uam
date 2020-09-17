package IDominio;

import Excepciones.ArgsDistintosFuncionesException;
import INodo.Funcion.Funcion;
import INodo.Funcion.FuncionMultiplicacion;
import INodo.Funcion.FuncionResta;
import INodo.Funcion.FuncionSuma;
import INodo.Terminal.Terminal;
import INodo.Terminal.TerminalAritmetico;
import IIndividuo.IIndividuo;

import java.io.*;
import java.util.*;

/**
 * Clase publica DominioAritmetico que hereda de la interfaz IDominio
 * @author Tomas HIguera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public class DominioAritmetico implements IDominio {

    private Map<Double, Double> valores = new TreeMap<>();

    /**
     * Metodo para definir una lista segun un conjunto de terminales
     * @param terminales Terminales con las que se creara la lista
     * @return Lista que contiene los terminales
     */
    @Override
    public List<Terminal> definirConjuntoTerminales(String... terminales) {
        List<Terminal> termis = new ArrayList<>();
        for(String operador: terminales){
            termis.add(new TerminalAritmetico(operador));
        }

        return termis;
    }

    /**
     * * Metodo para definir una lista segun un conjunto de terminales, si el numero de argumentos no coincide en numero
     * Se lanza la excepcion ArgsDistintosFuncionesException
     * @param argumentos Array que contiene el numero de argumentos de cada funcion
     * @param funciones Array que contiene todas las funciones en forma de String
     * @return Devuelve una List que contiene las funciones creadas
     * @throws ArgsDistintosFuncionesException Esta excepcion se lanza si no coincide el tamanio de funciones y argumentos
     */
    @Override
    public List<Funcion> definirConjuntoFunciones(int[] argumentos, String... funciones) throws ArgsDistintosFuncionesException {
        List<Funcion> funcs = new ArrayList<>();
        int i = 0;

        if(argumentos.length != funciones.length)
            throw new ArgsDistintosFuncionesException();

        for(String operador: funciones){
            i++;
            switch (operador) {
                case "x":
                    funcs.add(new FuncionMultiplicacion(operador, argumentos[i]));
                    break;
                case "+":
                    funcs.add(new FuncionSuma(operador, argumentos[i]));
                    break;
                case "-":
                    funcs.add(new FuncionResta(operador, argumentos[i]));
                    break;
                default:
                    return null;
            }
        }
        return funcs;
    }

    /**
     * Metodo de lectura de fichero para y para la definición de los valores de prueba y reales en el atributo valores
     * de la clase.
     * @param ficheroDatos Nombre del fichero de datos que contiene los valores de prueba
     * @throws FileNotFoundException Esta excepcion se lanzara cuando no exista el fichero de datos con el nombre especificao
     * @throws IOException Esta excepcion se lanzara si hay algun problema en la lectura del fichero
     */
    @Override
    public void definirValoresPrueba(String ficheroDatos) throws FileNotFoundException, IOException {
        String string;
        String[] vs;

        Scanner sc = new Scanner(new File(ficheroDatos));
        try {
            while ((string = sc.nextLine()) != null) {
                vs = string.split(" ");
                valores.put(Double.parseDouble(vs[0]), Double.parseDouble(vs[1]));
            }
        }catch (NoSuchElementException ignored){

        }
    }

    /**
     * Metodo para calcular el fitness de un individuo.
     * @param individuo Un objeto de tipo individuo que sera sometido a pruebas para ver su correcto funcionamiento
     * @return Se devolvera el fitness del individuo especificado
     */
    @Override
    public double calcularFitness(IIndividuo individuo) {
        double fitness = 0.0;

        for(double key : valores.keySet()){
            double vReal = valores.get(key);

            Terminal.valor = key;

            double resultado = individuo.getExpresion().calcular();

            if(resultado == vReal || resultado + 1 == vReal || resultado - 1 == vReal){
                fitness++;
            }

            System.out.println("Valor " + key + " <-> Rdo estimado: " + resultado + " <-> Rdo real: " + vReal);
        }
        individuo.setFitness(fitness);

        return fitness;
    }
}
