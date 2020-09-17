package es.uam.padsof.gui;

import es.uam.padsof.controlador.ControladorMainAdministrador;
import es.uam.padsof.gui.estilos.BotonAcciones;
import es.uam.padsof.gui.estilos.Styles;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.fechasimulada.FechaSimulada;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import javax.swing.*;
import java.util.ArrayList;

/**
 * Clase PanelMainAdministrador que muestra todos los paneles del panelMain de un administrador
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelMainAdministrador extends PanelMain {
    /**
     * Constructor de la clase PanelMainAdministrador
     * @param principal FramePrincipal que hace de contenedor principal del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    PanelMainAdministrador(FramePrincipal principal, Aplicacion aplicacion){
        super(principal, aplicacion);

        // Creamos los botones del panel de acciones
        BotonAcciones boton_proyectos_pendientes = new BotonAcciones("Proyectos pendientes");
        BotonAcciones boton_proyectos_rechazados = new BotonAcciones("Proyectos rechazados");
        BotonAcciones boton_proyectos_aceptados = new BotonAcciones("Proyectos aceptados");
        BotonAcciones boton_proyectos_caducados = new BotonAcciones("Proyectos caducados");
        BotonAcciones boton_proyectos_espera = new BotonAcciones("Proyectos en espera");
        BotonAcciones boton_proyectos_solicitables = new BotonAcciones("Proyectos solicitables");
        BotonAcciones boton_proyectos_financiados = new BotonAcciones("Proyectos financiados");
        BotonAcciones boton_usuarios_aceptados = new BotonAcciones("Usuarios aceptados");
        BotonAcciones boton_usuarios_bloqueados = new BotonAcciones("Usuarios bloqueados");
        BotonAcciones boton_umbral_votos = new BotonAcciones("Umbral de votos");
        BotonAcciones boton_avanzar_dias = new BotonAcciones("Avanzar dias");

        // Anadimos los controladores
        boton_proyectos_pendientes.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosPendientes()));
        boton_proyectos_rechazados.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosRechazados()));
        boton_proyectos_aceptados.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosAceptados()));
        boton_proyectos_caducados.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosCaducados()));
        boton_proyectos_solicitables.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosSolicitable()));
        boton_proyectos_espera.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosEspera()));
        boton_proyectos_financiados.addActionListener(actionEvent -> mostrarProyectos(aplicacion.getProyectosFinanciados()));
        boton_usuarios_aceptados.addActionListener(actionEvent -> mostrarUsuarios(aplicacion.getUsuariosAceptados()));
        boton_usuarios_bloqueados.addActionListener(actionEvent -> mostrarUsuarios(aplicacion.getUsuariosRechazados()));
        boton_umbral_votos.addActionListener(new ControladorMainAdministrador(this, aplicacion));
        boton_avanzar_dias.addActionListener(actionEvent -> {
            String dias = JOptionPane.showInputDialog(null, "Introduzca el numero de dias que quiere avanzar", null);
            if(dias != null)
                FechaSimulada.avanzar(Integer.parseInt(dias));
        });

        // Anadimos los botones al panel de acciones
        panel_acciones.add(boton_proyectos_pendientes, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_rechazados, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_aceptados, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_caducados, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_solicitables, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_espera, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_proyectos_financiados, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_usuarios_aceptados, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_usuarios_bloqueados, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_umbral_votos, "wrap 0, height 50, width 100%");
        panel_acciones.add(boton_avanzar_dias, "wrap 0, height 50, width 100%");
    }

    /**
     * Metodo que refresca el panelMainAdministrador para mostrar correctamente el panel de informacion
     */
    public void refrescar() {
        super.refrescar();
        panelTexto.removeAll();
        panelTexto.updateUI();
        texto_usuario.setText("Sesion iniciada como: Admin");
        JLabel umbral_votos = new JLabel("Umbral de votos: " + aplicacion.getNumero_votos());
        umbral_votos.setFont(Styles.getFont_label());
        panelTexto.add(texto_usuario, "align center");
        panelTexto.add(umbral_votos, "align center");
    }

    /**
     * Meotodo que muestra un listado de usuarios en el panel
     * @param usuarioRegistrados ArrayList que contiene los usuarios registrados que se mostraran
     */
    public void mostrarUsuarios(ArrayList<UsuarioRegistrado> usuarioRegistrados){
        layout_panelPrincipal.show(panel_principal, "listado");
        panelListado.refrescar_usuarios(usuarioRegistrados);
    }
}
