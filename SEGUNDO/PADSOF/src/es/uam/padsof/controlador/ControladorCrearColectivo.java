package es.uam.padsof.controlador;

import es.uam.padsof.gui.PanelCrearColectivo;
import es.uam.padsof.gui.PanelMainUsuario;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.AplicacionExcepcion;
import es.uam.padsof.modelo.excepcion.ColectivoExcepcion;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Clase ControladorCrearColectivo que implementa ActionListener y hace de controlador
 * de la vista PanelCrearColectivo
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class ControladorCrearColectivo implements ActionListener {
    private PanelMainUsuario panelMainUsuario;
    private Aplicacion aplicacion;
    private PanelCrearColectivo panelCrearColectivo;
    private Colectivo colectivo;

    /**
     * Constructor de la clase ControladorCrearColectivo
     * @param panelMainUsuario PanelMain donde se situa la vista
     * @param panelCrearColectivo PanelCrearColectivo que usa este controlador
     * @param aplicacion Aplicacion donde se contendra la informacion
     */
    public ControladorCrearColectivo(PanelMainUsuario panelMainUsuario, PanelCrearColectivo panelCrearColectivo, Aplicacion aplicacion){
        this.panelMainUsuario = panelMainUsuario;
        this.aplicacion = aplicacion;
        this.panelCrearColectivo = panelCrearColectivo;
        this.colectivo = null;
    }

    /**
     * Metodo que se ejcutara al hacer click en los botones que utilicen este controlador. Si todos los parametros
     * son correctos se creara un colectivo
     * @param actionEvent ActionEvent que se produce al hacer click en un boton
     */
    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        String nombre = panelCrearColectivo.getNombre();

        if(nombre.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir un nombre.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else{
            try {
                if (colectivo == null) {
                    aplicacion.crearColectivo(panelCrearColectivo.getNombre());
                    JOptionPane.showMessageDialog(null, "Colectivo creado con exito.");
                }else{
                    aplicacion.crearColectivoHijo(panelCrearColectivo.getNombre(), colectivo);
                    JOptionPane.showMessageDialog(null, "Colectivo creado con exito.");
                    panelMainUsuario.removeCustom(panelCrearColectivo);
                    panelMainUsuario.mostrar_elemento(colectivo);
                }
            } catch (ColectivoExcepcion | AplicacionExcepcion excepcion) {
                JOptionPane.showMessageDialog(null, "Error: " + excepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }

            panelCrearColectivo.cleanNombre();

        }
    }

    public void setColectivo(Colectivo colectivo){
        this.colectivo = colectivo;
    }
}
