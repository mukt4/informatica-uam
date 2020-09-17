package es.uam.padsof.gui;

import es.uam.padsof.controlador.ControladorLogin;
import es.uam.padsof.controlador.ControladorRegistro;
import es.uam.padsof.gui.estilos.Styles;
import es.uam.padsof.modelo.Aplicacion;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;

/**
 * Clase FramePrincipal que extiende de JFrame y actua de contenedor principal de las vistas de la aplicacion
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class FramePrincipal extends JFrame {
    private Aplicacion aplicacion;
    private PanelMainAdministrador panelMainAdministrador;
    private PanelMainUsuario panelMainUsuario;

    /**
     * Constructor de la clase FramePrincipal
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    public FramePrincipal(Aplicacion aplicacion){
        // Setteamos el nombre del JFrame
        super("APC principal");
        this.aplicacion = aplicacion;

        // Declaramos los componentes del frame principal
        JButton botonLogin = new JButton("Sign in");
        JButton botonRegistro = new JButton("Sign up");
        JLabel etiqueta = new JLabel("APC");
        etiqueta.setFont(Styles.getFont_logo_principal());
        botonLogin.setFont(Styles.getFont_botones());
        botonRegistro.setFont(Styles.getFont_botones());

        // Anadimos los controladores
        botonLogin.addActionListener(actionEvent -> mostrarLogin());
        botonRegistro.addActionListener(actionEvent -> mostrarRegistro());

        // Declaramos el contenedor prinicipal de la aplicacion
        Container contenedor = this.getContentPane();
        contenedor.setLayout(new CardLayout());

        // Creamos un panel que contendra los bootones de login y de registro
        JPanel panel_accion = new JPanel();
        panel_accion.setLayout(new MigLayout("filly"));
        panel_accion.add(botonLogin);
        panel_accion.add(botonRegistro, "gap left 20");
        panel_accion.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));

        JPanel panel_logo = new JPanel();
        panel_logo.setLayout(new MigLayout());
        panel_logo.add(etiqueta, "align center, push");

        // Creamos un panel que sera el que contendra los botones del panel principal
        JPanel principal = new JPanel();
        // Usare miglayout para hacer la aplicacion responsive
        principal.setLayout(new MigLayout());

        // Colocamos todos los componentes dentro del layout
        principal.add(panel_accion, "dock north, pushx, growx, height 70");
        principal.add(panel_logo, "align center, push, grow");


        // Creamos todos los paneles
        PanelLogin panelLogin = new PanelLogin(this);
        PanelRegistro panelRegistro = new PanelRegistro(this);
        panelMainUsuario = new PanelMainUsuario(this, aplicacion);
        panelMainAdministrador = new PanelMainAdministrador(this, aplicacion);

        // Anadimos los controladores de los paneles
        panelRegistro.setControlador(new ControladorRegistro(aplicacion, panelRegistro, this));
        panelLogin.setControlador(new ControladorLogin(aplicacion, panelLogin, this));

        // Anadimos los componenetes al contenedor
        add(panelLogin, "panelLogin");
        add(panelRegistro, "panelRegistro");
        add(panelMainAdministrador, "panelMainAdministrador");
        add(panelMainUsuario, "panelMainUsuario");
        add(principal, "panelPrincipal");

        // Por ultimo setteamos tanto tamano
        setMinimumSize(new Dimension(700, 650));
        setPreferredSize(new Dimension(1000, 650));
        setSize(1000, 650);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent windowEvent) {
                try {
                    aplicacion.guardarDatos();
                } catch (IOException e) {
                    JOptionPane.showMessageDialog(null, "Error en el guardado de la aplicacion.", "Error", JOptionPane.ERROR_MESSAGE);
                }
                System.exit(0);
            }
        });
        setResizable(true);

        // Mostramos el panel del menu principal
        mostrarPrincipal();
    }

    // A continuacion anadiremos todas las posibles metodos para mostrar views de nuestra aplicacion

    /**
     * Metodo que muestra el panelPrincipal de la aplicacion donde se encuentra el boton de login y de registro
     */
    public void mostrarPrincipal(){
        Container contenedor = this.getContentPane();
        CardLayout layout = (CardLayout) contenedor.getLayout();
        this.setTitle("Principal");
        layout.show(contenedor, "panelPrincipal");
    }

    /**
     * Metodo que muestra el panelRegistro de la aplicacion donde se encuentra el form de registro
     */
    private void mostrarRegistro(){
        Container contenedor = this.getContentPane();
        CardLayout layout = (CardLayout) contenedor.getLayout();
        this.setTitle("Sign up");
        layout.show(contenedor, "panelRegistro");
    }

    /**
     * Metodo que muestra el panelLogin donde se encuentra el form de login
     */
    private void mostrarLogin(){
        Container contenedor = this.getContentPane();
        CardLayout layout = (CardLayout) contenedor.getLayout();
        this.setTitle("Sign in");
        layout.show(contenedor, "panelLogin");
    }

    /**
     * Metodo que muestra el panel principal o bien de usuario registrado o bien de administrador
     * dependiendo de quien haga login
     */
    public void mostrarMain(){
        Container contenedor = this.getContentPane();
        CardLayout layout = (CardLayout) contenedor.getLayout();

        // Si el admin no  ha inciado sesion mostramos el menu principal de usuario normal
        if(aplicacion.getAdmin() != null){
            this.setTitle("Main administrador");
            layout.show(contenedor, "panelMainAdministrador");
            panelMainAdministrador.refrescar();
        }
        else {
            this.setTitle("Main usuario");
            layout.show(contenedor, "panelMainUsuario");
            panelMainUsuario.refrescar();
        }
    }
}
