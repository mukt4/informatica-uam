package es.uam.padsof.modelo.test;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.util.ArrayList;


/**
 * Clase que funciona como test de la clase usuarioRegistrado
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class UsuarioRegistradoTest {

	private UsuarioRegistrado usuario;

	/**
	 * Inicializacion de usuario de manera basica, asi como inicializacion de los objetos que seran utilizados en el test
	 */
	@Before
	public void setup(){
		usuario = new UsuarioRegistrado("01160720W","pezEspada","AnaMaria");
	}

	/**
	 * Tester que comprueba si el bloqueo de un usuario funciona correctamente
	 */
	@Test
	public void bloquearUsuario() {
		usuario.bloqueado();
		assertEquals(true, usuario.isBloqueado());
	}

	/**
	 * Tester que comprueba si el el metodo de checkeo de bloqueo de un usuario funciona correctamente
	 */
	@Test
	public void isBloqueado() {
		usuario.bloqueado();
		assertEquals(true, usuario.isBloqueado());
		usuario.aceptado();
		assertEquals(false, usuario.isBloqueado());
	}

	/**
	 * Metodo que comprueba si el metodo de limpiar notificaciones de un usuario funciona correctamente
	 */
	@Test
	public void limpiarNotificaciones() {
		ArrayList<String> notificaciones = new ArrayList<>();
		notificaciones.add("1");
		notificaciones.add("2");
		usuario.setNotificaciones(notificaciones);
		assertEquals(usuario.getNotificaciones().size(), 2);
		usuario.limpiarNotificaciones();
		assertEquals(usuario.getNotificaciones().size(),0);
	}

}
