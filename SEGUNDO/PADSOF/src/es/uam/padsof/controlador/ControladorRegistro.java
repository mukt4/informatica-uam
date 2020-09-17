package es.uam.padsof.controlador;

import es.uam.padsof.gui.FramePrincipal;
import es.uam.padsof.gui.PanelRegistro;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.excepcion.UsuarioExcepcion;
import es.uam.padsof.modelo.validar.ValidarDni;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Clase ControladorRegistro que implementa ActionListener y hace de controlador
 * de la vista PanelRegistro
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class ControladorRegistro implements ActionListener{
    private Aplicacion aplicacion;
    private PanelRegistro panelRegistro;
    private FramePrincipal framePrincipal;

    /**
     * Constructor de la clase ControladorRegistro
     * @param aplicacion Aplicacion que contiene toda la informacion
     * @param panelRegistro PanelRegistro donde se utiliza el controlador
     * @param framePrincipal FramePrincipal que hace de contenedor del resto de paneles
     */
    public ControladorRegistro(Aplicacion aplicacion, PanelRegistro panelRegistro, FramePrincipal framePrincipal){
        this.aplicacion = aplicacion;
        this.panelRegistro = panelRegistro;
        this.framePrincipal = framePrincipal;
    }

    /**
     * Metodo que se ejecuta al hacer click en el boton que utiliza este controlador. Si los parametros
     * son correctos se registra a un usuario en la aplicacion
     * @param actionEvent ActionEvent que se produce al hacer click en el boton
     */
    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        String nombre = panelRegistro.getNombre();
        String dni = panelRegistro.getDni();
        String contrasena = panelRegistro.getContrasena();

        if(nombre.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir un nombre.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else if(dni.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir un dni.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else if(contrasena.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir una contraseña.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else{
            try {
                ValidarDni validador = new ValidarDni(dni);
                // Validamos si el dni tiene el formato correcto
                if(validador.validar()) {
                    aplicacion.registrar(nombre, dni, contrasena);
                    // Si el registro se ha realizado con exito vamos a la ventana principal donde el usuario debera iniciar sesion
                    JOptionPane.showMessageDialog(null, "Registrado con exito.");
                    // Limpiamos el panel
                    panelRegistro.cleanNombre();
                    panelRegistro.cleanDni();
                    panelRegistro.cleanPassword();

                    framePrincipal.mostrarPrincipal();
                }
                else{
                    JOptionPane.showMessageDialog(null, "El dni tiene un formato incorrecto.", "Error", JOptionPane.ERROR_MESSAGE);
                    // Limpiamos el dni
                    panelRegistro.cleanDni();
                }
            } catch (UsuarioExcepcion usuarioExcepcion) {
                JOptionPane.showMessageDialog(null, usuarioExcepcion.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
