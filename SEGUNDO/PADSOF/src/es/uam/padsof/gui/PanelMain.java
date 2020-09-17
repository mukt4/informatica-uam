package es.uam.padsof.gui;

import es.uam.padsof.gui.estilos.BotonApp;
import es.uam.padsof.gui.estilos.BotonLogOut;
import es.uam.padsof.gui.estilos.Styles;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.proyecto.Proyecto;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

/**
 * Clase abstracta PanelMain que contiene todos los paneles del panelMain de los usuarios
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public abstract class PanelMain extends JPanel{
    Aplicacion aplicacion;
    JPanel panel_acciones;
    JPanel panel_principal;
    JLabel texto_usuario;
    PanelListado panelListado;
    private PanelElemento panelElemento;
    CardLayout layout_panelPrincipal;
    JPanel panelTexto;

    /**
     * Constructor de la clase PanelMain
     * @param principal FramePrincipal que hace de contenedor del resto de clases
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    PanelMain(FramePrincipal principal, Aplicacion aplicacion){
        this.aplicacion = aplicacion;
        BotonApp botonApp = new BotonApp();
        botonApp.addActionListener(actionEvent -> principal.mostrarMain());
        BotonLogOut botonLogOut = new BotonLogOut();
        botonLogOut.addActionListener(actionEvent -> {
            aplicacion.logout();
            principal.mostrarPrincipal();
        });

        // Este panel contendra el texto de informacion de usuario
        panelTexto = new JPanel();
        panelTexto.setLayout(new MigLayout("fillx"));
        texto_usuario = new JLabel();
        texto_usuario.setFont(Styles.getFont_label());
        panelTexto.add(texto_usuario, "align center");

        // Este panel contendra el header con la informacion de usuario
        JPanel panel_informacion = new JPanel();
        panel_informacion.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.black));
        panel_informacion.setLayout(new MigLayout());
        panel_informacion.add(botonApp, "dock west, gaptop 12, gapleft 12");
        panel_informacion.add(botonLogOut, "dock east");
        panel_informacion.add(panelTexto, "pushx, growx");

        // Este panel contendra las acciones posibles del usuario
        JPanel panel_acciones_contenedor = new JPanel();
        panel_acciones_contenedor.setLayout(new MigLayout("fill, insets 0 0 0 0"));
        panel_acciones_contenedor.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.black));
        panel_acciones = new JPanel();
        panel_acciones.setLayout(new MigLayout("fillx, insets 0 0 0 0"));
        panel_acciones_contenedor.add(panel_acciones,"dock north");

        // Este panel contendra la informacion principal
        panel_principal = new JPanel();
        layout_panelPrincipal = new CardLayout();
        panel_principal.setLayout(layout_panelPrincipal);

        // Anadimos los posibles paneles del panel principal
        panelListado = new PanelListado(this, aplicacion);
        panel_principal.add(panelListado, "listado");
        panelElemento = new PanelElemento(this, aplicacion);
        JScrollPane scrollable_elemento = new JScrollPane(panelElemento);
        scrollable_elemento.setBorder(BorderFactory.createEmptyBorder());
        panel_principal.add(scrollable_elemento, "elemento");

        // Organizamos los botones del menu principals
        setLayout(new MigLayout("fill, insets 0 0 0 0"));
        add(panel_informacion, "dock north, height 70, pushx, growx");
        add(panel_acciones_contenedor, "dock west, width 350");
        add(panel_principal, "push, grow");
    }

    /**
     * Metodo que refresca el panel main y muestra todos los proyectos disponibles
     */
    public void refrescar(){
        mostrarProyectos(aplicacion.getProyectosDisponibles());
    }

    /**
     * Metodo que muestra un listado de colectivos en el panel
     * @param colectivos ArrayList de colectivos que se mostraran
     */
    void mostrarColectivos(ArrayList<Colectivo> colectivos){
        layout_panelPrincipal.show(panel_principal, "listado");
        panelListado.refrescar_colectivos(colectivos);
    }

    /**
     * Metodo que muestra un listado de proyectos en el panel
     * @param proyectos ArrayList de proyectos que se mostraran
     */
    void mostrarProyectos(ArrayList<Proyecto> proyectos){
        aplicacion.comprobarVotos();
        aplicacion.comprobarCaducados();
        aplicacion.comprobarProyectosEspera();
        layout_panelPrincipal.show(panel_principal, "listado");
        panelListado.refrescar_proyectos(proyectos);
    }

    /**
     * Metodo que muestra un elemento en el panel
     * @param colectivo Colectivo que se mostrara
     */
    public void mostrar_elemento(Colectivo colectivo){
        layout_panelPrincipal.show(panel_principal, "elemento");
        panelElemento.refrescar(colectivo);
    }

    /**
     * Metodo que muestra un elemento en el panel
     * @param proyecto Proyecto que se mostrara
     */
    public void mostrar_elemento(Proyecto proyecto){
        layout_panelPrincipal.show(panel_principal, "elemento");
        panelElemento.refrescar(proyecto);
    }
}
