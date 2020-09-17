package IIndividuo;

import Excepciones.CruceNuloException;
import INodo.Funcion.Funcion;
import INodo.INodo;
import INodo.Terminal.Terminal;

import java.util.List;

/**
 * Interfaz publica IIndividuo
 * @author Tomas Higuera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public interface IIndividuo {
        INodo getExpresion();
        void setExpresion(INodo expresion);
        double getFitness();
        void setFitness(double fitness);
        void crearIndividuoAleatorio(int profundidad, List<Terminal> terminales, List<Funcion> funciones);
        double calcularExpresion();
        int getNumeroNodos();
        void writeIndividuo();
        void etiquetaNodos();
        INodo getNodoEtiqueta(int etiqueta) throws CruceNuloException;
        INodo getPadreNodoEtiqueta(int etiqueta) throws CruceNuloException;
}
