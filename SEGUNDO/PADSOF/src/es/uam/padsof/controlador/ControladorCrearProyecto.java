package es.uam.padsof.controlador;

import es.uam.padsof.gui.PanelCrearProyecto;
import es.uam.padsof.gui.PanelMainUsuario;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.ProyectoExcepcion;
import es.uam.padsof.modelo.fechasimulada.FechaSimulada;
import es.uam.padsof.modelo.proyecto.Ambito;
import es.uam.padsof.modelo.proyecto.Distrito;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;

/**
 * Clase ControladorCrearProyecto que implementa ActionListener y hace de controlador
 * de la vista PanelCrearProyecto
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class ControladorCrearProyecto implements ActionListener {
    private PanelCrearProyecto panelCrearProyecto;
    private Aplicacion aplicacion;
    private Colectivo colectivo;
    private PanelMainUsuario panelMainUsuario;

    /**
     * Constructor de la clase ControladorCrearProyecto
     * @param panelCrearProyecto PanelCrearProyecto donde se ejecuta el controlador
     * @param aplicacion Aplicacion donde se ejecutaran las acciones del controlador
     * @param panelMainUsuario PanelMainUsuario que hace de contenedor principal
     */
    public ControladorCrearProyecto(PanelCrearProyecto panelCrearProyecto, Aplicacion aplicacion, PanelMainUsuario panelMainUsuario){
        this.panelCrearProyecto = panelCrearProyecto;
        this.aplicacion = aplicacion;
        this.panelMainUsuario = panelMainUsuario;
        this.colectivo = null;
    }

    /**
     * Metodo que se ejecutara al hacer click en el boton que utiliza este controlador. Si los parametros son correctos
     * se creara un proyecto social o de tipo infraestructura
     * @param actionEvent ActionEvent que se produce al hacer click en el boton
     */
    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        Enumeration<AbstractButton> botones = panelCrearProyecto.getBotones_tipo_proyecto().getElements();;
        String nombre = panelCrearProyecto.getNombre();
        String descripcion = panelCrearProyecto.getDescripcion();
        String importe_str = panelCrearProyecto.getImporte();
        String grupoSocial = panelCrearProyecto.getGrupoSocial();
        Ambito ambito = panelCrearProyecto.getAmbito();
        File esquemaGrafico = panelCrearProyecto.getEsquemaGrafico();

        double importe;

        if(nombre.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir un nombre.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else if(descripcion.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir una descripcion.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else if(importe_str.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir un importe valido.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else{
            try {
                importe = Double.valueOf(importe_str);
                if(importe < 0){
                    JOptionPane.showMessageDialog(null, "Debe introducir un importe positivo.", "Error", JOptionPane.ERROR_MESSAGE);
                    panelCrearProyecto.clean_importe();
                }else{
                    if(botones.nextElement().isSelected()){
                        if(esquemaGrafico == null){
                            JOptionPane.showMessageDialog(null, "Debe anadir un esquema grafico.", "Error", JOptionPane.ERROR_MESSAGE);
                        }else{
                            ArrayList<Distrito> distritos = panelCrearProyecto.getDistritos();
                            if(distritos.size() == 0){
                                JOptionPane.showMessageDialog(null, "Debe seleccionar al menos un distrito", "Error", JOptionPane.ERROR_MESSAGE);
                            }else{
                                try {
                                    if (colectivo == null) {
                                        aplicacion.crearProyectoInfraestructura(nombre, descripcion, importe, distritos, esquemaGrafico, FechaSimulada.getHoy());
                                        JOptionPane.showMessageDialog(null, "Proyecto creado con exito.");
                                        panelCrearProyecto.clean();
                                    }else{
                                        aplicacion.crearProyectoInfraestructura(nombre, descripcion, importe, colectivo, distritos, esquemaGrafico, FechaSimulada.getHoy());
                                        JOptionPane.showMessageDialog(null, "Proyecto creado con exito.");
                                        panelMainUsuario.removeCustom(panelCrearProyecto);
                                        panelMainUsuario.mostrar_elemento(colectivo);
                                    }
                                }catch (ProyectoExcepcion excepcion){
                                    JOptionPane.showMessageDialog(null, "Error: " + excepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                                }
                            }
                        }
                    }else if(botones.nextElement().isSelected()){
                        if(grupoSocial.equals("")){
                            JOptionPane.showMessageDialog(null, "Debe introducir un grupo social.", "Error", JOptionPane.ERROR_MESSAGE);
                        }
                        else {
                            try {
                                if(colectivo == null){
                                    aplicacion.crearProyectoSocial(nombre, descripcion, importe, grupoSocial, ambito, FechaSimulada.getHoy());
                                    JOptionPane.showMessageDialog(null, "Proyecto creado con exito.");
                                    panelCrearProyecto.clean();
                                } else{
                                    aplicacion.crearProyectoSocial(nombre, descripcion, importe, colectivo, grupoSocial, ambito, FechaSimulada.getHoy());
                                    JOptionPane.showMessageDialog(null, "Proyecto creado con exito.");
                                    panelMainUsuario.removeCustom(panelCrearProyecto);
                                    panelMainUsuario.mostrar_elemento(colectivo);
                                }
                            }catch(ProyectoExcepcion excepcion){
                                JOptionPane.showMessageDialog(null, "Error: " + excepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
                            }
                        }
                    }else{
                        JOptionPane.showMessageDialog(null, "Debe seleccionar un tipo de proyecto", "Error", JOptionPane.ERROR_MESSAGE);
                    }
                }
            }catch (NumberFormatException excepcion){
                JOptionPane.showMessageDialog(null, "Debe introducir un importe valido", "Error", JOptionPane.ERROR_MESSAGE);
                panelCrearProyecto.clean_importe();
            }
        }
    }

    public void setColectivo(Colectivo colectivo){
        this.colectivo = colectivo;
    }
}
