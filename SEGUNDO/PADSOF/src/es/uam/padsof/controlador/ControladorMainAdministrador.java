package es.uam.padsof.controlador;

import es.uam.padsof.gui.PanelMainAdministrador;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.excepcion.AplicacionExcepcion;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Clase ControladorMainAdministrador que implementa ActionListener y hace de controlador
 * de la vista PanelMainAdministrador
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class ControladorMainAdministrador implements ActionListener {
    private PanelMainAdministrador panelMainAdministrador;
    private Aplicacion aplicacion;

    /**
     * Constructor de la clase PanelMainAdministrador
     * @param panelMainAdministrador PanelMainAdministrador que hace de contenedor del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    public ControladorMainAdministrador(PanelMainAdministrador panelMainAdministrador, Aplicacion aplicacion){
        this.panelMainAdministrador = panelMainAdministrador;
        this.aplicacion = aplicacion;
    }

    /**
     * Metodo que se ejecuta al hacer click en un boton que utiliza este controlador. Este metodo solo cambia el umbral
     * de votos de la aplicacion
     * @param actionEvent ActionEvent que se produce al hacer click en el boton
     */
    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        String votos = JOptionPane.showInputDialog(null, "Introduzca el umbral de votos", null);
        if(votos != null){
            try{
                int num_votos = Integer.valueOf(votos);
                try {
                    aplicacion.setNumero_votos(num_votos);
                    JOptionPane.showMessageDialog(null, "Umbral de votos cambiado con exito");
                    panelMainAdministrador.refrescar();
                } catch (AplicacionExcepcion aplicacionExcepcion) {
                    JOptionPane.showMessageDialog(null, "Error: " + aplicacionExcepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                }
            } catch (NumberFormatException excepcion){
                JOptionPane.showMessageDialog(null, "El umbral de votos debe ser un numero entero", "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
