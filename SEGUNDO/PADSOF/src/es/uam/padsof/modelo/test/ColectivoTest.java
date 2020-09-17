package es.uam.padsof.modelo.test;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

/**
 * Clase que funciona como test de la clase colectivo
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class ColectivoTest {

	private UsuarioRegistrado usuario;
	private UsuarioRegistrado usuario2;
	private Colectivo colectivo;

	/**
	 * Inicializacion de colectivo de manera basica, asi como inicializacion de los objetos que seran utilizados en el test
	 */
	@Before
	public void setup(){
		usuario = new UsuarioRegistrado("23933652J","asdasd","Sergio");
		usuario2 = new UsuarioRegistrado("04290368V", "1234", "Tom");
		colectivo = new Colectivo("Colectivo 1", usuario);
	}

	/**
	 * Test que comprueba si el metodo crear colectivo hijo funciona correctamentes
	 */
	@Test
	public void crearColectivoHijo1() {
		assertEquals(true, colectivo.crearColectivoHijo("hijo1"));
		assertEquals(true, colectivo.crearColectivoHijo("hijo2"));
		assertEquals(2, colectivo.getColectivosHijo().size());
	}

	/**
	 * Test que comprueba si no se pueden crear dos hijos con el mismo nombre
	 */
	@Test
	public void crearColectivoHijo2() {
		assertEquals(true, colectivo.crearColectivoHijo("hijo1"));
		assertEquals(false, colectivo.crearColectivoHijo("hijo1"));
		assertEquals(1, colectivo.getColectivosHijo().size());
	}


	/**
	 * Test que comprueba si se puede seguir un colectivo como usuario correctamente
	 */
	@Test
	public void seguirColectivo1() {
		assertEquals(true, colectivo.seguirColectivo(usuario2));
	}

	/**
	 * Test que comprueba si no se puede seguir un colectivo si ya lo estas siguiendo
	 */
	@Test
	public void seguirColectivo2() {
		colectivo.seguirColectivo(usuario2);
		assertEquals(false, colectivo.seguirColectivo(usuario2));
	}

	/**
	 * Test que comprueba si se puede abandonar un colectivo correctamente
	 */
	@Test
	public void abandonarColectivo1() {
		colectivo.seguirColectivo(usuario2);
		assertEquals(true, colectivo.abandonarColectivo(usuario2));
	}

	/**
	 * Test que comprueba si se no puede abandonar un colectivo que no sigues
	 */
	@Test
	public void abandonarColectivo2() {
		assertEquals(false, colectivo.abandonarColectivo(usuario2));
	}

	/**
	 * Test que comprueba si un nombre pertenece a un colectivo hijo
	 */
	@Test
	public void comprobarNombre1() {
		colectivo.crearColectivoHijo("hijo1");
		colectivo.crearColectivoHijo("hijo3");
		assertEquals(true, colectivo.comprobarNombre("hijo1"));
	}

	/**
	 * Test que comprueba si un nombre no pertenece a un colectivo hijo
	 */
	@Test
	public void comprobarNombre2() {
		colectivo.crearColectivoHijo("hijo1");
		colectivo.crearColectivoHijo("hijo3");
		assertEquals(false, colectivo.comprobarNombre("hijo2"));
	}

	/**
	 * Test que comprueba el creador de un colectivo
	 */
	@Test
	public void testComprobarCreador() {
		assertEquals(true, colectivo.comprobarCreador(usuario.getNombreUsuario()));
	}

}
