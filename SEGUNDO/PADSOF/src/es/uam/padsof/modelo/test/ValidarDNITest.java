package es.uam.padsof.modelo.test;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import es.uam.padsof.modelo.validar.ValidarDni;

/**
 * Clase que funciona como test de la clase validarDni
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class ValidarDNITest {

	private ValidarDni dni1;
	private ValidarDni dni2;

	/**
	 * Inicializacion de validarDni de manera basica, asi como inicializacion de los objetos que seran utilizados en el test
	 */
	@Before
	public void setup(){
		dni1 = new ValidarDni("545372M");
		dni2 = new ValidarDni("01160720W");
	}

	/**
	 * Test que comprueba si la clase validarDni valida un DNI de manera correcta
	 */
	@Test
	public void validar1() {
		assertEquals(true,dni2.validar());
	}

	/**
	 * Tester que comprueba que la clase validarDni no valida un dni incorrecto
	 */
	@Test
	public void validar2() {
		assertEquals(false,dni1.validar());
	}

}
