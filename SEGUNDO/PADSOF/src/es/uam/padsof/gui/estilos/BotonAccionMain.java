package es.uam.padsof.gui.estilos;

import javax.swing.*;

/**
 * Clase BotonAccionMain que extiende de JButton y se utiliza en los botones de la parte superior
 * del panelMain
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class BotonAccionMain extends JButton {
    /**
     * Constructor de la clase BotonAccionMain
     * @param nombre String que contiene el nombre que se escribira en el boton
     */
    public BotonAccionMain(String nombre){
        super(nombre);
        setFont(Styles.getFont_botones());
    }
}
