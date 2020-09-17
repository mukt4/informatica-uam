package es.uam.padsof.gui;

import es.uam.padsof.gui.estilos.BotonAcciones;
import es.uam.padsof.modelo.Aplicacion;
import net.miginfocom.swing.MigLayout;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.io.File;

/**
 * Clase PanelMainUsuario que contiene toda la informacion del panelMain de un usuario registrador
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelMainUsuario extends PanelMain {
    private JPanel panel_contenedor_notificaciones;

    /**
     * Constructor de la clase PanelMainUsuario
     * @param principal FramePrincipal que hace de contenedor del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    PanelMainUsuario(FramePrincipal principal, Aplicacion aplicacion){
        super(principal, aplicacion);

        // Creamos y anadimos el boton de notificaciones al panel que hace de header
        JButton notificaciones_boton = new JButton();
        panel_contenedor_notificaciones = new JPanel();
        panel_contenedor_notificaciones.setLayout(new MigLayout());
        String dir = System.getProperty("user.dir") + "/media/notificacion.png";
        try {
            Image img = ImageIO.read(new File(dir));
            notificaciones_boton.setIcon(new ImageIcon(img));
        } catch (Exception ex) {
            System.out.println("Error al cargar la aplicacion: " + dir);
        }
        notificaciones_boton.addActionListener(actionEvent -> mostrar_notificaciones());
        notificaciones_boton.setFocusPainted(false);
        notificaciones_boton.setMargin(new Insets(0, 0, 0, 0));
        notificaciones_boton.setContentAreaFilled(false);
        notificaciones_boton.setOpaque(false);
        notificaciones_boton.setBorder(BorderFactory.createEmptyBorder());
        panel_contenedor_notificaciones.add(notificaciones_boton);

        // Creo los diferentes botones del panel de acciones
        BotonAcciones boton_proyectos = new BotonAcciones("Todos los proyectos");
        BotonAcciones boton_colectivos = new BotonAcciones("Todos los colectivos");
        BotonAcciones boton_mis_proyectos = new BotonAcciones("Mis proyectos");
        BotonAcciones boton_mis_colectivos = new BotonAcciones("Mis colectivos");
        BotonAcciones boton_colectivos_seguidos = new BotonAcciones("Colectivos seguidos");
        BotonAcciones boton_proyectos_seguidos = new BotonAcciones("Proyectos seguidos");
        BotonAcciones boton_proyectos_votados = new BotonAcciones("Proyectos votados");

        // Anadimos los controladores
        boton_proyectos.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosDisponibles()));
        boton_colectivos.addActionListener(actionEvent -> mostrarColectivos(aplicacion.getColectivos()));
        boton_mis_proyectos.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getMisProyectos()));
        boton_mis_colectivos.addActionListener(actionEvent -> mostrarColectivos(aplicacion.getMisColectivos()));
        boton_colectivos_seguidos.addActionListener(actionEvent -> mostrarColectivos(aplicacion.getColectivosSeguidos()));
        boton_proyectos_seguidos.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosSeguidos()));
        boton_proyectos_votados.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getMisVotados()));

        // Anadimos los botones
        panel_acciones.add(boton_proyectos, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_colectivos, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_mis_proyectos, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_mis_colectivos, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_seguidos, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_colectivos_seguidos, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_votados, "wrap 0, height 50, width 100%");

        // Anadimos los paneles que son exclusivos del menu principal de usuario
        PanelCrearColectivo panelCrearColectivo = new PanelCrearColectivo(this, aplicacion);
        panel_principal.add(panelCrearColectivo, "crearColectivo");
        PanelCrearProyecto panelCrearProyecto = new PanelCrearProyecto(this, aplicacion);
        panel_principal.add(panelCrearProyecto, "crearProyecto");
    }

    /**
     * Metodo que refresca el panelMainUsuario para mostrar correctamente el panel de informacion
     */
    public void refrescar(){
        super.refrescar();
        panelTexto.removeAll();
        panelTexto.updateUI();
        texto_usuario.setText("Sesion iniciada como: " + aplicacion.getUsuarioActual().getNombreUsuario());
        panelTexto.add(texto_usuario, "align center");
        panelTexto.add(panel_contenedor_notificaciones, "right, gapright 15");
    }

    /**
     * Metodo que muestra el panel de crear un colectivo
     */
    public void mostrar_crearColectivo() {
        layout_panelPrincipal.show(panel_principal, "crearColectivo");
    }

    /**
     * Metodo que muestra el panel de crear proyecto
     */
    public void mostrar_crearProyecto() {
        layout_panelPrincipal.show(panel_principal, "crearProyecto");
    }

    /**
     * Metodo que muestra el panel de crear proyecto desde un colectivo
     * @param panelCrearProyecto PanelCrearProyecto customizado para poder crear desde un colectivo
     */
    public void mostrarCustom(PanelCrearProyecto panelCrearProyecto){
        panel_principal.add(panelCrearProyecto, "custom");
        layout_panelPrincipal.show(panel_principal, "custom");
    }

    /**
     * Metodo que elimina el panel de crear proyecto desde un colectivo
     * @param panelCrearProyecto PanelCrearProyecto customizado que se eliminara
     */
    public void removeCustom(PanelCrearProyecto panelCrearProyecto){
        panel_principal.remove(panelCrearProyecto);
    }

    /**
     * Metodo que muestra el panel de crear colectivo desde un colectivo
     * @param panelCrearColectivo PanelCrearColectivo customizado para poder crear desde un colectivo
     */
    public void mostrarCustom(PanelCrearColectivo panelCrearColectivo){
        panel_principal.add(panelCrearColectivo, "custom");
        layout_panelPrincipal.show(panel_principal, "custom");
    }

    /**
     * Metodo que elimina el panel de crear colectivo desde un colectivo
     * @param panelCrearColectivo PanelCrearColectivo customizado que se eliminara
     */
    public void removeCustom(PanelCrearColectivo panelCrearColectivo){
        panel_principal.remove(panelCrearColectivo);
    }

    /**
     * Metodo que muestra el listado de notificaciones del usuario
     */
    public void mostrar_notificaciones(){
        layout_panelPrincipal.show(panel_principal, "listado");
        panelListado.refrescar_notificaciones();
    }


}
