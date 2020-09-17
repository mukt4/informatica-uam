package es.uam.padsof.gui.estilos;

import javax.swing.*;
import java.awt.*;

/**
 * Clase BotonAcciones que extiende de JButton y se utiliza en el panel lateral de acciones del PanelMain
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class BotonAcciones extends JButton {
    /**
     * Constructor de la clase BotonAcciones
     * @param nombre String que contiene el texto que se escribira en el boton
     */
    public BotonAcciones(String nombre){
        super("    " + nombre);
        setFocusPainted(false);
        setMargin(new Insets(0, 0, 0, 0));
        setContentAreaFilled(false);
        setOpaque(false);
        setFont(Styles.getFont_acciones());
        setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));
        setHorizontalAlignment(SwingConstants.LEFT);
    }
}
