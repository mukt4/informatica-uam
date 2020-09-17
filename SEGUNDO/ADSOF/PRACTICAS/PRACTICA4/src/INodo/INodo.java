package INodo;

import java.util.List;

/**
 * Interfaz publica INodo
 * @author Tomas Higuera Viso y Guillermo Hoyo Bravo
 * @version 1.0
 */
public interface INodo {
    String getRaiz();
    List<INodo> getDescendientes();
    void incluirDescendiente(INodo nodo);
    double calcular();
    INodo copy();
    int getNnodos();
    int getEtiqueta();
    void setEtiqueta(int etiqueta);
    void setExpresion(String expresion);
    void setNHijos(int hijos);
    int getNHijos();
    void setNodos(List<INodo> nodos);
}
