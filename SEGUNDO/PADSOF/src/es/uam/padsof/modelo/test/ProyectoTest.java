package es.uam.padsof.modelo.test;

import static org.junit.Assert.*;

import es.uam.padsof.modelo.fechasimulada.FechaSimulada;
import es.uam.padsof.modelo.proyecto.Ambito;
import es.uam.padsof.modelo.proyecto.ProyectoSocial;
import org.junit.Before;
import org.junit.Test;

import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.ProyectoExcepcion;
import es.uam.padsof.modelo.proyecto.Proyecto;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.util.Random;


/**
 * Clase que funciona como test de la clase proyecto
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class ProyectoTest {

	private Proyecto proyecto;
	private UsuarioRegistrado usuario;
	private UsuarioRegistrado usuario2;
	private Colectivo colectivo;

    /**
     * Inicializacion de proyecto de manera basica, asi como inicializacion de los objetos que seran utilizados en el test
     */
	@Before
	public void setup(){
	    Ambito ambito = Ambito.INTERNACIONAL;
		usuario = new UsuarioRegistrado("23933652J","asdasd","Sergio");
		usuario2 = new UsuarioRegistrado("04290368V", "1234", "Tom");
		proyecto = new ProyectoSocial("Proyecto 1", "Este es el proyecto 1", 25, usuario, FechaSimulada.getHoy(), "Este es el grupo social del proyecto 1", ambito);
	    colectivo = new Colectivo("Colectivo 1", usuario);
	}

    /**
     * Tester que comprueba si el metodo seguir proyecto funciona correctamente
     */
	@Test
	public void seguirProyectoUsuarioRegistrado1() {
		assertEquals(true, proyecto.seguirProyecto(usuario2));
	}

    /**
     * Tester que comprueba si se seguir un proyecto como colectivo funciona correctamente
     */
	@Test
	public void seguirProyectoColectivo1() {
		assertEquals(true, proyecto.seguirProyecto(colectivo));
	}

    /**
     * Tester que comprueba si una vez seguido un proyecto como colectivo se puede volver a seguir
     */
    @Test
    public void seguirProyectoColectivo2() {
        proyecto.seguirProyecto(colectivo);
        assertEquals(false, proyecto.seguirProyecto(colectivo));
    }

    /**
     * Tester que comprueba si un usuario puede dejar de seguir un proyecto correctamente
     */
    @Test
	public void abandonarProyectoUsuarioRegistrado1() {
        proyecto.seguirProyecto(usuario2);
		assertEquals(true, proyecto.abandonarProyecto(usuario2));
	}

    /**
     * Tester que comprueba si un un usuario que no sigue un proyecto puede abandonarlo
     */
    @Test
    public void abandonarProyectoUsuarioRegistrado2() {
        assertEquals(false, proyecto.abandonarProyecto(usuario2));
    }

    /**
     * Tester que comprueba si un colectivo puede dejar de seguir un proyecto correctamente
     */
	@Test
	public void abandonarProyectoColectivo1() {
        proyecto.seguirProyecto(colectivo);
		assertEquals(true, proyecto.abandonarProyecto(colectivo));
	}

    /**
     * Tester que comprueba si un colectivo que no sigue un proyecto puede abandonarlo
     */
    @Test
    public void abandonarProyectoColectivo2() {
        assertEquals(false, proyecto.abandonarProyecto(colectivo));
    }

    /**
     * Tester que comprueba si un usuario es seguidor de un proyecto
     */
	@Test
	public void comprobarSeguidor1() {
	    proyecto.seguirProyecto(usuario2);
		assertEquals(true, proyecto.comprobarSeguidor(usuario2));
	}

    /**
     * Tester que comprueba si un usuario no es seguidor de un proyecto
     */
    @Test
    public void comprobarSeguidor2() {
        assertEquals(false, proyecto.comprobarSeguidor(usuario2));
    }

    /**
     * Tester que comrpueba si un colectivo es seguidor de un proyecto
     */
	@Test
	public void comprobarColectivoSeguidor1() {
		proyecto.seguirProyecto(colectivo);
		assertEquals(true, proyecto.comprobarColectivoSeguidor(colectivo));
	}

    /**
     * Tester que comrpueba si un colectivo no es seguidor de un proyecto
     */
    @Test
    public void comprobarColectivoSeguidor2() {
        assertEquals(false, proyecto.comprobarColectivoSeguidor(colectivo));
    }

    /**
     * Tester que comprueba si un usuario es creador de un proyecto
     */
	@Test
	public void comprobarCreador1() {
		assertEquals(true, proyecto.comprobarCreador(usuario));
	}

    /**
     * Tester que comprueba si un usuario no es creador de un proyecto
     */
    @Test
    public void comprobarCreador2() {
        assertEquals(false, proyecto.comprobarCreador(usuario2));
    }

    /**
     * Tester que comrpueba si un proyecto se puede rechazar con exito
     */
	@Test
	public void rechazarProyecto1(){
        Exception excepcion = null;
        try {
            proyecto.rechazarProyecto("hola");
        } catch (ProyectoExcepcion proyectoExcepcion) {
            excepcion = proyectoExcepcion;
        }
        assertNull(excepcion);
        assertEquals(true, proyecto.isRechazado());
	}

    /**
     * Tester que comrpueba si un proyecto no es rechazado al no cumplir los requisitos
     */
    @Test
    public void rechazarProyecto2(){
        int leftLimit = 97; // letter 'a'
        int rightLimit = 122; // letter 'z'
        int targetStringLength = 51;
        Random random = new Random();
        Exception excepcion = null;

        String generatedString = random.ints(leftLimit, rightLimit + 1)
                .limit(targetStringLength)
                .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
                .toString();

        try {
            proyecto.rechazarProyecto(generatedString);
        } catch (ProyectoExcepcion proyectoExcepcion) {
            excepcion = proyectoExcepcion;
        }
        assertNotNull(excepcion);
        assertEquals(false, proyecto.isRechazado());
    }

    /**
     * Tester que comprueba si el numero de votos de un proyecto es correcto
     */
    @Test
	public void getVotos() {
		proyecto.votarProyecto(usuario2);
		colectivo.seguirColectivo(usuario2);
		proyecto.votarProyecto(colectivo);
		assertEquals(2, proyecto.getVotos());
	}

    /**
     * Tester que comprueba si el cambio a aceptado de un proyecto funciona correctamente
     */
	@Test
	public void aceptado() {
		proyecto.aceptado();
		assertEquals(true, proyecto.isAceptado());
	}

    /**
     * Tester que comprueba si el cambio a pendiente de un proyecto funciona correctamente
     */
	@Test
	public void pendiente() {
		proyecto.pendiente();
		assertEquals(true, proyecto.isPendiente());
	}

    /**
     * Tester que comprueba si el cambio a rechazado de un proyecto funciona correctamente
     */
	@Test
	public void rechazado() {
		proyecto.rechazado("because");
		assertEquals(true, proyecto.isRechazado());
	}

    /**
     * Tester que commrueba si el cambio a caducado de un proyecto funciona correctamente
     */
	@Test
	public void caducado() {
		proyecto.caducado();
		assertEquals(true, proyecto.isCaducado());
	}

    /**
     * Tester que comprueba si el cambio a financiado de un proyecto funciona correctamente
     */
	@Test
	public void financiado() {
		proyecto.financiado(10.00);
		assertEquals(true, proyecto.isFinanciado());
	}

    /**
     * Tester que comprueba si el cambio a espera de un proyecto funciona correctamente
     */
	@Test
	public void peticion() {
		proyecto.peticion("please");
		assertEquals(true, proyecto.isEspera());
		assertEquals(false, proyecto.isFinanciado());
	}

    /**
     * Test que comprueba si un cambio en el proyecto notifica a los usuarios correctamente
     */
	@Test
    public void notificar(){
        proyecto.seguirProyecto(usuario);
        proyecto.financiado(25);
        assertEquals(1, usuario.getNotificaciones().size());
    }

}
