package IAlgoritmo;

import Excepciones.CruceNuloException;
import INodo.Funcion.Funcion;
import INodo.Terminal.Terminal;
import IDominio.IDominio;
import IIndividuo.IIndividuo;

import java.util.*;

/**
 * Interfaz publica IAlgoritmo
 * @author Tomas Higuera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public interface IAlgoritmo {
    void defineConjuntoTerminales(List<Terminal> terminales);
    void defineConjuntoFunciones(List<Funcion> funciones);
    void crearPoblacion();
    List<IIndividuo> cruce(IIndividuo prog1, IIndividuo prog2) throws CruceNuloException;
    void crearNuevaPoblacion();
    void ejecutar(IDominio dominio);
}
