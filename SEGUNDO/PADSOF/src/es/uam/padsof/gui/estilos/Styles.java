package es.uam.padsof.gui.estilos;

import java.awt.*;

/**
 * Clase Styles que contiene la hoja de estilos de la aplicacion
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class Styles {
    /**
     * Metodo estatico que devuelve la Font del logo principal
     * @return Font de logo de la aplicacion
     */
    public static Font getFont_logo_principal() {
        return new Font(Font.SERIF, Font.PLAIN, 100);
    }

    /**
     * Metodo estatico que devuelve la Font de los botones de la aplicacion
     * @return Font de los botones de la aplicacion
     */
    public static Font getFont_botones() {
        return new Font("Arial", Font.BOLD, 25);
    }

    /**
     * Metodo estatico que devuelve la Font del logo secundario
     * @return Font secundario del logo de la aplicacion
     */
    public static Font getFont_logo(){
        return new Font(Font.SERIF, Font.PLAIN, 65);
    }

    /**
     * Metodo estatico que devuele la Font de los labels de la aplicacion
     * @return Font de los labels
     */
    public static Font getFont_label(){
        return new Font("Arial", Font.PLAIN, 25);
    }

    /**
     * Metodo que devuelve la Font del los botones del panel de acciones
     * @return Font de los botones del panel de acciones
     */
    public static Font getFont_acciones(){
        return new Font("Arial", Font.BOLD, 23);
    }
}
