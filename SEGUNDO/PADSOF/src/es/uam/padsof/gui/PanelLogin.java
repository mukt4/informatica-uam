package es.uam.padsof.gui;

import es.uam.padsof.gui.estilos.BotonApp;
import es.uam.padsof.gui.estilos.Styles;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;

/**
 * Clase PanelLogin que muestra el formulario de login de la aplicacion
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelLogin extends JPanel{
    private JButton boton;
    private JTextField nombre;
    private JPasswordField contrasena;

    /**
     * Constructor de la clase PanelLogin
     * @param principal FramePrincipal que hace de contendor del resto de paneles
     */
    PanelLogin(FramePrincipal principal){
        // Creamos los labels
        JLabel nombreLabel = new JLabel("Nombre");
        nombreLabel.setFont(Styles.getFont_botones());
        JLabel contrasenaLabel = new JLabel("Contrasena");
        contrasenaLabel.setFont(Styles.getFont_botones());

        // Creamos el boton de la aplicacion
        BotonApp boton_app = new BotonApp();
        boton_app.addActionListener(actionEvent -> principal.mostrarPrincipal());

        // Creamos los textField
        this.nombre = new JTextField(20);
        nombre.setFont(Styles.getFont_label());
        this.contrasena = new JPasswordField(20);
        contrasena.setFont(Styles.getFont_label());

        // Creamos los botones
        this.boton = new JButton("Sign in");
        boton.setFont(Styles.getFont_botones());

        // Creamos los paneles que contendra los componentes
        JPanel panel_boton = new JPanel();
        panel_boton.setLayout(new MigLayout());
        panel_boton.add(boton_app);

        JPanel panel_central1 = new JPanel();
        panel_central1.setLayout(new MigLayout("fill"));
        JPanel panel_central = new JPanel();
        panel_central.setLayout(new MigLayout("wrap 1, insets 20 20 20 20"));
        panel_central.setBorder(BorderFactory.createLineBorder(Color.black));

        panel_central.add(nombreLabel);
        panel_central.add(nombre, "wrap 10");
        panel_central.add(contrasenaLabel);
        panel_central.add(contrasena, "wrap 20");
        panel_central.add(boton);
        panel_central1.add(panel_central, "align center");

        // Setteamos el tipo de layout y colocamos los componentes
        setLayout(new MigLayout("wrap 1"));
        add(panel_boton);
        add(panel_central1, "push, grow");
    }

    // Getters y setters

    /**
     * Setter del controlador de la vista
     * @param controlador ActionListener que hace de controlador de la lista
     */
    public void setControlador(ActionListener controlador){
        this.boton.addActionListener(controlador);
    }

    /**
     * Getter del nombre del formulario
     * @return String que contiene el nombre de usuario
     */
    public String getNombre(){
        return nombre.getText();
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
     * Metodo que limpia el JTextField que contiene el nombre de usuario
     */
    public void cleanNombre(){
        nombre.setText("");
    }

    /**
     * Metodo que limpia el JPasswordField que contiene la contrasena del usuario
     */
    public void cleanPassword(){
        contrasena.setText("");
    }

}
