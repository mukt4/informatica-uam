package es.uam.padsof.controlador;

import es.uam.padsof.gui.FramePrincipal;
import es.uam.padsof.gui.PanelLogin;
import es.uam.padsof.modelo.Aplicacion;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Clase ControladorLogin que implementa ActionListener y hace de controlador
 * de la vista PanelLogin
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class ControladorLogin implements ActionListener {
    private Aplicacion aplicacion;
    private PanelLogin panelLogin;
    private FramePrincipal framePrincipal;

    /**
     * Constructor de la clase ControladorLogin
     * @param aplicacion Aplicacion que contiene toda la informacion
     * @param panelLogin PanelLogin que utiliza el controlador
     * @param framePrincipal FramePrincipal que contiene el resto de paneles
     */
    public ControladorLogin(Aplicacion aplicacion, PanelLogin panelLogin, FramePrincipal framePrincipal){
        this.aplicacion = aplicacion;
        this.panelLogin = panelLogin;
        this.framePrincipal = framePrincipal;
    }

    /**
     * Metodo que se ejecuta al hacer click en boton que utiliza este controlador. Si todos los parametros son correctos
     * un usuario incia sesion en la aplicacion
     * @param actionEvent ActionEvent que se produce al hacer click en el boton
     */
    @Override
    public void actionPerformed(ActionEvent actionEvent) {
        String nombre = panelLogin.getNombre();
        String contrasena = panelLogin.getContrasena();

        if(nombre.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir un nombre.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else if(contrasena.equals("")){
            JOptionPane.showMessageDialog(null, "Debe introducir una contraseña.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else{
            // Si nos logeamos correctamente mostramos el panel main de la aplicacion
            if(aplicacion.login(nombre, contrasena)){
                framePrincipal.mostrarMain();
            }
            else{
                JOptionPane.showMessageDialog(null, "Nombre o contrasena incorrectos.", "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
        panelLogin.cleanNombre();
        panelLogin.cleanPassword();
    }
}
