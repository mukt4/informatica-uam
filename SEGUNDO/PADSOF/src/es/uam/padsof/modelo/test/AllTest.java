package es.uam.padsof.modelo.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Clase que corre todos los test unitarios de la aplicacion
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
        AplicacionTest.class,
        ColectivoTest.class,
        ProyectoTest.class,
        UsuarioRegistradoTest.class,
        ValidarDNITest.class
})

public class AllTest {
}
