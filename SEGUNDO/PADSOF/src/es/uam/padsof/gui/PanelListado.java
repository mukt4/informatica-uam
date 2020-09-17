package es.uam.padsof.gui;

import es.uam.padsof.controlador.ControladorListado;
import es.uam.padsof.gui.estilos.BotonAccionMain;
import es.uam.padsof.gui.estilos.Styles;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.proyecto.Proyecto;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

/**
 * Clase PanelListado que muestra un listado de elementos, o bien usuarios, colectivos, proyectos o notificaciones
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelListado extends JPanel {
    // Creamos los dos posibles botones de crear
    private BotonAccionMain boton_accion;
    // Creamos los dos posibles paneles
    private JPanel panel_acciones;
    private JPanel panel_listado;
    private PanelMain principal;
    private JScrollPane scrollable;
    private Aplicacion aplicacion;

    /**
     * Constructor de la clase PanelListado
     * @param principal PanelMain que hace de contenedor del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    PanelListado(PanelMain principal, Aplicacion aplicacion){
        this.principal = principal;
        this.aplicacion = aplicacion;

        // Seteamos el tipo de layout del panel
        setLayout(new MigLayout("insets 0 0 0 0"));

        // Creamos los botones posibles
        boton_accion = new BotonAccionMain("Crear proyecto");
        boton_accion.addActionListener(new ControladorListado(principal, aplicacion));

        // Creamos el panel que contendra las posibles acciones
        panel_acciones = new JPanel();
        panel_acciones.setLayout(new MigLayout());
        panel_acciones.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));
        panel_acciones.add(boton_accion, "dock west, height 100%");

        // Creamos el panel que contendra el listado que se mostrara
        panel_listado = new JPanel();
        panel_listado.setLayout(new MigLayout("fillx, insets 0 0 0 0"));

        // Creamos tambien el panel scrollable
        scrollable = new JScrollPane(panel_listado);

        scrollable.setBorder(BorderFactory.createMatteBorder(0, 0, 0,0, Color.black));
    }

    /**
     * Metodo que refresca el panel y muestra una lista de colectivos
     * @param colectivos ArrayList que contiene los colectivos que se mostraran
     */
    public void refrescar_colectivos(ArrayList<Colectivo> colectivos){
        removeAll();
        updateUI();
        if(aplicacion.getAdmin() == null)
            add(panel_acciones, "height 50, pushx, growx, wrap 0");
        add(scrollable, "push, grow");
        boton_accion.setVisible(true);
        boton_accion.setText("Crear colectivo");
        panel_listado.removeAll();
        panel_listado.updateUI();

        if (colectivos.size() != 0) {
            for(Colectivo colectivo : colectivos){
                // Cada panel contendra informacion acerca de cada colectivos
                JPanel panel = new JPanel();
                panel.setLayout(new MigLayout());
                JLabel nombre_etiqueta = new JLabel("Nombre: ");
                nombre_etiqueta.setFont(Styles.getFont_botones());
                JLabel nombre = new JLabel(colectivo.getNombre());
                nombre.setFont(Styles.getFont_label());

                panel.add(nombre_etiqueta, "wrap 1, gapleft 10");
                panel.add(nombre, "gapleft 10");
                panel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));

                panel.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent mouseEvent) {
                        principal.mostrar_elemento(colectivo);
                    }
                });

                panel_listado.add(panel, "pushx, growx, wrap 0");
            }
        }
        else{
            JLabel no_colectivos = new JLabel("No hay colectivos disponibles");
            no_colectivos.setFont(Styles.getFont_label());
            panel_listado.add(no_colectivos, "gapleft 10, gaptop 10");
        }
    }

    /**
     * Metodo que refresca el panel y muestra una lista de proyectos
     * @param proyectos ArrayList que contiene los proyectos que se mostraran
     */
    public void refrescar_proyectos(ArrayList<Proyecto> proyectos){
        removeAll();
        updateUI();

        if(aplicacion.getAdmin() == null)
            add(panel_acciones, "height 50, pushx, growx, wrap 0");
        add(scrollable, "push, grow");
        boton_accion.setVisible(true);
        boton_accion.setText("Crear proyecto");
        panel_listado.removeAll();
        panel_listado.updateUI();
        if (proyectos.size() != 0) {
            for(Proyecto proyecto : proyectos){
                // Cada panel contendra informacion acerca de cada colectivos
                JPanel panel = new JPanel();
                panel.setLayout(new MigLayout());
                JLabel nombre_etiqueta = new JLabel("Nombre: ");
                nombre_etiqueta.setFont(Styles.getFont_botones());
                JLabel nombre = new JLabel(proyecto.getProjectTitle());
                JLabel descripcion_etiqueta = new JLabel("Descripcion: ");
                descripcion_etiqueta.setFont(Styles.getFont_botones());
                JLabel descripcion = new JLabel(proyecto.getProjectDescription());
                descripcion.setFont(Styles.getFont_label());
                nombre.setFont(Styles.getFont_label());

                panel.add(nombre_etiqueta, "gapleft 10");
                panel.add(nombre, "gapleft 10, wrap");
                panel.add(descripcion_etiqueta, "gapleft 10");
                panel.add(descripcion, "gapleft 10");

                panel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));

                panel.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent mouseEvent) {
                        principal.mostrar_elemento(proyecto);
                    }
                });

                panel_listado.add(panel, "pushx, growx, wrap 0");
            }
        }
        else{
            JLabel no_proyectos = new JLabel("No hay proyectos disponibles");
            no_proyectos.setFont(Styles.getFont_label());
            panel_listado.add(no_proyectos, "gapleft 10, gaptop 10");
        }
    }

    /**
     * Metodo que refresca el panel y muestra una lista de usuario
     * @param usuarios ArrayList que contiene la lista de usuarios
     */
    public void refrescar_usuarios(ArrayList<UsuarioRegistrado> usuarios){
        removeAll();
        updateUI();
        add(scrollable, "push, grow");
        panel_listado.removeAll();
        panel_listado.updateUI();
        boton_accion.setVisible(false);
        if (usuarios.size() != 0) {
            for(UsuarioRegistrado usuario : usuarios){
                JPanel panel = new JPanel();
                panel.setLayout(new MigLayout("fillx, insets 0 0 0 0"));
                panel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));
                JLabel nombre_label = new JLabel("Nombre: ");
                nombre_label.setFont(Styles.getFont_botones());
                JLabel nombre = new JLabel(usuario.getNombreUsuario());
                nombre.setFont(Styles.getFont_label());
                JButton boton_usuario = new JButton();
                boton_usuario.setFont(Styles.getFont_botones());
                boton_usuario.addActionListener(new ControladorListado(principal, usuario));
                if(usuario.isBloqueado()){
                    boton_usuario.setText("Aceptar");
                }
                else{
                    boton_usuario.setText("Bloquear");
                }
                JPanel panel_info = new JPanel();
                panel_info.setLayout(new MigLayout());
                panel_info.add(nombre_label);
                panel_info.add(nombre);
                panel.add(panel_info);
                panel.add(boton_usuario, "right");
                panel_listado.add(panel, "pushx, growx, wrap 0");
            }
        }
        else{
            JLabel no_usuarios = new JLabel("No hay usuarios en la aplicacion");
            no_usuarios.setFont(Styles.getFont_label());
            panel_listado.add(no_usuarios,"gapleft 10, gaptop 10");
        }
    }

    /**
     * Metodo que refresca el panel y muestra las notificaciones del usuario actual de la aplicacion
     */
    public void refrescar_notificaciones(){
        ArrayList<String> notificaciones = aplicacion.getUsuarioActual().getNotificaciones();
        removeAll();
        updateUI();
        if (aplicacion.getAdmin() == null && notificaciones.size() != 0) {
            add(panel_acciones, "height 50, pushx, growx, wrap 0");
            add(scrollable, "push, grow");
            boton_accion.setVisible(true);
            boton_accion.setText("Limpiar notificaciones");
            panel_listado.removeAll();
            panel_listado.updateUI();

            for(String notificacion : notificaciones){
                JPanel panel_contenedor = new JPanel();
                panel_contenedor.setLayout(new MigLayout());
                panel_contenedor.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));
                JLabel notificacion_label = new JLabel(notificacion);
                notificacion_label.setFont(Styles.getFont_label());
                panel_contenedor.add(notificacion_label);
                panel_listado.add(panel_contenedor, "pushx, growx, wrap 0");
            }
        }
        else{
            add(scrollable, "push, grow");
            boton_accion.setVisible(false);
            panel_listado.removeAll();
            panel_listado.updateUI();
            JLabel no_notificaciones = new JLabel("No hay nuevas notificaciones");
            no_notificaciones.setFont(Styles.getFont_label());
            panel_listado.add(no_notificaciones, "gapleft 10, gaptop 10");
        }

    }
}
