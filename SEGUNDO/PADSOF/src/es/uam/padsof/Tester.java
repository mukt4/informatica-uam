package es.uam.padsof;

import es.uam.padsof.gui.FramePrincipal;
import es.uam.padsof.modelo.Aplicacion;

import java.io.IOException;

public class Tester {
    public static void main(String[] args) throws IOException, ClassNotFoundException {
        Aplicacion aplicacion = new Aplicacion();
        FramePrincipal ventana = new FramePrincipal(aplicacion);
        ventana.setVisible(true);
        ventana.setLocationRelativeTo(null);
    }
}
