package es.uam.padsof.gui;

import es.uam.padsof.gui.estilos.BotonApp;
import es.uam.padsof.gui.estilos.Styles;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;

/**
 * Clase PanelRegistro que contiene el formulario de registro de la aplicacion
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelRegistro extends JPanel {
    private JButton boton;
    private JTextField nombre;
    private JTextField dni;
    private JPasswordField contrasena;

    /**
     * Constructor de la clase PanelRegistro
     * @param principal FramePrincipal que hace de contenedor del resto de paneles
     */
    PanelRegistro(FramePrincipal principal) {
        // Primero creamos los componentes
        // Setteamos los labels
        JLabel nombreLabel = new JLabel("Nombre");
        nombreLabel.setFont(Styles.getFont_botones());
        JLabel contrasenaLabel = new JLabel("Contrasena");
        contrasenaLabel.setFont(Styles.getFont_botones());
        JLabel dniLabel = new JLabel("DNI");
        dniLabel.setFont(Styles.getFont_botones());

        // Creamos el boton del logo
        BotonApp boton_app = new BotonApp();
        boton_app.addActionListener(actionEvent -> principal.mostrarPrincipal());

        // Setteamos los textFields
        this.nombre = new JTextField(20);
        nombre.setFont(Styles.getFont_label());
        this.dni = new JTextField(20);
        dni.setFont(Styles.getFont_label());
        this.contrasena = new JPasswordField(20);
        contrasena.setFont(Styles.getFont_label());

        // Setteamos el boton
        this.boton = new JButton("Sign up");
        boton.setFont(Styles.getFont_botones());

        // Creamos los paneles para contener los componentes
        JPanel panel_central1 = new JPanel();
        panel_central1.setLayout(new MigLayout("fill"));

        JPanel panel_central = new JPanel();
        panel_central.setLayout(new MigLayout("wrap 1"));
        panel_central.setBorder(BorderFactory.createLineBorder(Color.black));

        // Anadimos el panel creado
        panel_central.add(nombreLabel);
        panel_central.add(nombre, "wrap 10");
        panel_central.add(dniLabel);
        panel_central.add(dni, "wrap 10");
        panel_central.add(contrasenaLabel);
        panel_central.add(contrasena, "wrap 20");
        panel_central.add(boton);

        panel_central1.add(panel_central, "align center");

        // Setteamos el layout y anadimos los componentes
        setLayout(new MigLayout("wrap 1"));
        add(boton_app);
        add(panel_central1, "push, grow");
    }

    // Getters y setters

    /**
     * Setter del controlador de la vista
     * @param controlador ActionListener que hace de controlador de la vista
     */
    public void setControlador(ActionListener controlador){
        this.boton.addActionListener(controlador);
    }

    /**
     * Getter del nombre de usuario del formulario
     * @return String que contiene el nombre de usuario
     */
    public String getNombre(){
        return nombre.getText();
    }

    /**
     * Getter del dni del formulario
     * @return String que contiene el dni del usuario
     */
    public String getDni(){
        return dni.getText();
    }

    /**
     * Getter de la contrasena del formulario
     * @return String que contiene la contrasena del usuario
     */
    @Deprecated
    public String getContrasena(){
        return contrasena.getText();
    }

    /**
     * Metodo que limpia el JTextField del nombre de usuario
     */
    public void cleanNombre(){
        nombre.setText("");
    }

    /**
     * Metodo que limpia el JPasswordField de la contrasena
     */
    public void cleanPassword(){
        contrasena.setText("");
    }

    /**
     * Metodo que limpia el JTextField del dni
     */
    public void cleanDni(){
        dni.setText("");
    }
}
