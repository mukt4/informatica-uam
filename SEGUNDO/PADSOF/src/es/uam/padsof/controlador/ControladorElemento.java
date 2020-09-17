package es.uam.padsof.controlador;

import es.uam.eps.sadp.grants.InvalidIDException;
import es.uam.eps.sadp.grants.InvalidRequestException;
import es.uam.padsof.gui.*;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.AplicacionExcepcion;
import es.uam.padsof.modelo.proyecto.Proyecto;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

/**
 * Clase ControladorElemento que implementa ActionListener y hace de controlador
 * de la vista PanelElemento
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class ControladorElemento implements ActionListener {
    private Aplicacion aplicacion;
    private PanelElemento panelElemento;
    private PanelMain panelMain;

    /**
     * Constructor de la clase ControladorElemento
     * @param aplicacion Aplicacion donde se ejecutaran las accion
     * @param panelElemento PanelElemento en el que se utiliza el controlador
     * @param panelMain PanelMain que hace de contenedor del resto de panels
     */
    public ControladorElemento(Aplicacion aplicacion, PanelElemento panelElemento, PanelMain panelMain){
        this.aplicacion = aplicacion;
        this.panelElemento = panelElemento;
        this.panelMain = panelMain;
    }

    /**
     * Metodo que se ejecuta al hacer click en un boton que utiliza este controlador. Las posibles acciones son:
     * Seguir proyecto desde colectivo o desde usuario
     * Abandonar proyecto
     * Seguir colectivo
     * Abandonar colectivo
     * Votar proyecto
     * Crear proyecto desde un colectio
     * Crear colectivo hijo desde un colectivo
     * Consultar afinidad
     * Aceptar
     * Rechazar
     * Solicitar financiacion
     * Comprobar financiacion
     * @param actionEvent ActionEvent que se ejecutara al hacer click en el boton
     */
    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        Colectivo colectivo = panelElemento.getColectivo();
        Proyecto proyecto = panelElemento.getProyecto();

        if(aplicacion.getUsuarioActual() != null && aplicacion.getUsuarioActual().isBloqueado()){
            JOptionPane.showMessageDialog(null, "Un usuario bloqueado no puede realizar esta accion.", "Error", JOptionPane.ERROR_MESSAGE);
        } else {
            switch (actionEvent.getActionCommand()) {
                case "Seguir proyecto":
                    if (colectivo == null) {
                        proyecto.seguirProyecto(aplicacion.getUsuarioActual());
                        JOptionPane.showMessageDialog(null, "Proyecto seguido con exito.");
                        panelElemento.refrescar();
                    } else{
                        try {
                            panelElemento.getSelectedProyecto().seguirProyecto(colectivo);
                            JOptionPane.showMessageDialog(null, "Proyecto seguido con exito.");
                            panelElemento.refrescar();
                        } catch (AplicacionExcepcion aplicacionExcepcion) {
                            JOptionPane.showMessageDialog(null, "Error: " + aplicacionExcepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                        }
                    }
                    break;

                case "Abandonar proyecto":
                    if (colectivo == null) {
                        proyecto.abandonarProyecto(aplicacion.getUsuarioActual());
                        JOptionPane.showMessageDialog(null, "Proyecto abandonado con exito.");
                        panelElemento.refrescar();
                    }
                    else{
                        try {
                            panelElemento.getSelectedProyecto().abandonarProyecto(colectivo);
                            JOptionPane.showMessageDialog(null, "Proyecto abandonado con exito.");
                            panelElemento.refrescar();
                        } catch (AplicacionExcepcion aplicacionExcepcion) {
                            JOptionPane.showMessageDialog(null, "Error: " + aplicacionExcepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                        }
                    }
                    break;

                case "Seguir colectivo":
                    try {
                        aplicacion.seguirColectivo(colectivo);
                        JOptionPane.showMessageDialog(null, "Colectivo seguido con exito.");
                        panelElemento.refrescar();
                    } catch (AplicacionExcepcion aplicacionExcepcion) {
                        JOptionPane.showMessageDialog(null, "Error: " + aplicacionExcepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                    }
                    break;

                case "Abandonar colectivo":
                    colectivo.abandonarColectivo(aplicacion.getUsuarioActual());
                    JOptionPane.showMessageDialog(null, "Colectivo abandonado con exito con exito.");
                    panelElemento.refrescar();
                    break;

                case "Votar proyecto":
                    proyecto.votarProyecto(aplicacion.getUsuarioActual());
                    JOptionPane.showMessageDialog(null, "Proyecto votado con exito.");
                    panelElemento.refrescar();
                    break;

                case "Crear proyecto":
                    PanelCrearProyecto panelCrearProyecto = new PanelCrearProyecto((PanelMainUsuario)panelMain, aplicacion, colectivo);
                    ((PanelMainUsuario)panelMain).mostrarCustom(panelCrearProyecto);
                    break;

                case "Crear colectivo hijo":
                    PanelCrearColectivo panelCrearColectivo = new PanelCrearColectivo((PanelMainUsuario)panelMain, aplicacion, colectivo);
                    ((PanelMainUsuario)panelMain).mostrarCustom(panelCrearColectivo);
                    break;

                case "Consultar afinidad":
                    try {
                        double afinidad = aplicacion.consultarAfinidad(colectivo, panelElemento.getSelectedColectivo());
                        JOptionPane.showMessageDialog(null, "Afinidad entre colectivos: " + afinidad);
                    } catch (AplicacionExcepcion aplicacionExcepcion) {
                        JOptionPane.showMessageDialog(null, "Error: " + aplicacionExcepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                    }
                    break;

                case "Aceptar":
                    proyecto.aceptado();
                    JOptionPane.showMessageDialog(null, "El proyecto ha sido aceptado con exito");
                    panelElemento.refrescar();
                    break;

                case "Rechazar":
                    String rechazo = JOptionPane.showInputDialog(null, "Introduzca el motivo de rechazo", null);
                    if(rechazo != null){
                        proyecto.rechazado(rechazo);
                        JOptionPane.showMessageDialog(null, "El proyecto ha sido rechazado con exito");
                        panelElemento.refrescar();
                    }
                    break;

                case "Solicitar financiacion":
                    try {
                        aplicacion.solicitarFinanciacion(proyecto);
                        JOptionPane.showMessageDialog(null, "El proyecto ha sido enviado a financiacion");
                        panelElemento.refrescar();
                    } catch (AplicacionExcepcion aplicacionExcepcion) {
                        JOptionPane.showMessageDialog(null, "Error aplicacion: " + aplicacionExcepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                    } catch (IOException | InvalidRequestException e) {
                        JOptionPane.showMessageDialog(null, "Error del servidor: " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                    }
                    break;

                case "Comprobar financiacion":
                    try {
                        aplicacion.comprobarFinanciacionProyecto(proyecto);
                        JOptionPane.showMessageDialog(null, "El proyecto ha sido financiado");
                    } catch (AplicacionExcepcion aplicacionExcepcion) {
                        JOptionPane.showMessageDialog(null, aplicacionExcepcion.getMessage());
                    } catch (IOException | InvalidIDException e) {
                        JOptionPane.showMessageDialog(null, "Error del servidor: " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                    }
                    break;

            }
        }

    }
}
