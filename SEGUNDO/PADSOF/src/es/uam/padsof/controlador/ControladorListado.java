package es.uam.padsof.controlador;

import es.uam.padsof.gui.PanelMainAdministrador;
import es.uam.padsof.gui.PanelMainUsuario;
import es.uam.padsof.gui.PanelMain;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Clase ControladorListado que implementa ActionListener y hace de controlador
 * de la vista PanelListado
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class ControladorListado implements ActionListener {
    private PanelMain panelMain;
    private UsuarioRegistrado usuarioRegistrado;
    private Aplicacion aplicacion;

    /**
     * Constructor de la clase ControladorListado
     * @param panelMain PanelMain que hace de contenedor principal
     * @param usuarioRegistrado UsuarioRegistrado que aparece en el listado
     */
    public ControladorListado(PanelMain panelMain, UsuarioRegistrado usuarioRegistrado){
        this.panelMain = panelMain;
        this.usuarioRegistrado = usuarioRegistrado;
        this.aplicacion = null;
    }

    /**
     * Constructor de la clase ControladorListado
     * @param panelMain PanelMainn que hace de contenedor principal
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    public ControladorListado(PanelMain panelMain, Aplicacion aplicacion){
        this.panelMain = panelMain;
        this.usuarioRegistrado = null;
        this.aplicacion = aplicacion;
    }

    /**
     * Metodo que se ejecuta al hacer click en los botones que utilizan este controlador. Las acciones que puede
     * llevar acabo son:
     * Crear proyecto, que te lleva a la vista
     * Crear colectivo, que te lleva a la vista
     * Aceptar a un usuario registrado
     * Rechazar a un usuario registrador
     * Limpiar notificaciones del usuario actual de la aplicacion
     * @param actionEvent ActionEvent que se ejecuta al hacer click en el boton
     */
    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        switch (actionEvent.getActionCommand()) {
            case "Crear colectivo":
                ((PanelMainUsuario) panelMain).mostrar_crearColectivo();
                break;

            case "Crear proyecto":
                ((PanelMainUsuario) panelMain).mostrar_crearProyecto();
                break;

            case "Aceptar":
                usuarioRegistrado.aceptado();
                JOptionPane.showMessageDialog(null, "Usuario aceptado con exito.");
                ((PanelMainAdministrador) panelMain).mostrarUsuarios(aplicacion.getUsuariosRechazados());
                break;

            case "Bloquear":
                usuarioRegistrado.bloqueado();
                JOptionPane.showMessageDialog(null, "Usuario bloqueado con exito.");
                ((PanelMainAdministrador) panelMain).mostrarUsuarios(aplicacion.getUsuariosAceptados());
                break;

            case "Limpiar notificaciones":
                aplicacion.getUsuarioActual().limpiarNotificaciones();
                JOptionPane.showMessageDialog(null, "Notificaciones limpiadas con exito");
                ((PanelMainUsuario)panelMain).mostrar_notificaciones();
                break;
        }
    }
}
