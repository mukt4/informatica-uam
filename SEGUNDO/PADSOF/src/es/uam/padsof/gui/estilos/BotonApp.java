package es.uam.padsof.gui.estilos;

import javax.swing.*;
import java.awt.*;

/**
 * Clase BotonApp que extiende de JButton y que se utiliza para representar el boton con el logo de la aplicacion
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class BotonApp extends JButton {

    /**
     * Constructor de la clase BotonApp
     */
    public BotonApp(){
        super("APC");
        setFocusPainted(false);
        setMargin(new Insets(0, 0, 0, 0));
        setContentAreaFilled(false);
        setBorderPainted(false);
        setOpaque(false);
        setFont(Styles.getFont_logo());
    }
}
