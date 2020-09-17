package es.uam.padsof.gui.estilos;

import javax.swing.*;

/**
 * Clase BotonLogOut que extiende de JButton y que se utiliza para representar el boton
 * de log out de la aplicacion
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class BotonLogOut extends JButton {
    /**
     * Constructor de la clase BotonLogOut
     */
    public BotonLogOut(){
        super("Log out");
        setFont(Styles.getFont_botones());
    }
}
