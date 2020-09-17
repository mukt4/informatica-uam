package IDominio;

import Excepciones.ArgsDistintosFuncionesException;
import INodo.Funcion.Funcion;
import INodo.Terminal.Terminal;
import IIndividuo.IIndividuo;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

/**
 * Interfaz publica IDominio
 * @author Guillermo Hoyo Bravo y Tomas Higuera Viso
 * @version 1.0
 */
public interface IDominio {
    List<Terminal> definirConjuntoTerminales(String... terminales);
    List<Funcion> definirConjuntoFunciones(int[] argumentos, String... funciones) throws
            ArgsDistintosFuncionesException;
    void definirValoresPrueba(String ficheroDatos) throws FileNotFoundException, IOException;
    double calcularFitness(IIndividuo individuo);
}