package es.uam.padsof.gui;

import es.uam.padsof.controlador.ControladorCrearColectivo;
import es.uam.padsof.gui.estilos.Styles;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;

/**
 * Clase PanelCrearColectivo que contiene el formulario para crear un colectivo
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelCrearColectivo extends JPanel {
    private JTextField nombre_colectivo;
    private JButton boton_crear;

    /**
     * Constructor de la clase PanelCrearColectivo
     * @param panelMainUsuario PanelMainUsuario que hace de contenedor del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     * @param colectivo Colectivo si se ejecuta el panel desde otro colectivo para crear colectivo hijo
     */
    public PanelCrearColectivo(PanelMainUsuario panelMainUsuario, Aplicacion aplicacion, Colectivo colectivo){
        this(panelMainUsuario, aplicacion);
        ((ControladorCrearColectivo)boton_crear.getActionListeners()[0]).setColectivo(colectivo);
    }

    /**
     * Constructor de la clase PanelCrearColectivo
     * @param panelMainUsuario PanelMainUsuario que contiene el resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    PanelCrearColectivo(PanelMainUsuario panelMainUsuario, Aplicacion aplicacion){
        // Creamos label y textField
        JLabel nombre = new JLabel("Nombre: ");
        nombre.setFont(Styles.getFont_botones());
        nombre_colectivo = new JTextField(15);
        nombre_colectivo.setFont(Styles.getFont_label());

        // Cremos el boton junto con su controlador
        boton_crear = new JButton("Crear colectivo");
        boton_crear.setFont(Styles.getFont_botones());
        boton_crear.addActionListener(new ControladorCrearColectivo(panelMainUsuario, this, aplicacion));

        // Setteamos el layout y anadimos los componentes
        setLayout(new MigLayout("insets 20 20 0 0, wrap 2"));
        add(nombre);
        add(nombre_colectivo);
        add(boton_crear, "skip");
    }

    public String getNombre(){
        return nombre_colectivo.getText();
    }

    public void cleanNombre(){
        nombre_colectivo.setText("");
    }
}
