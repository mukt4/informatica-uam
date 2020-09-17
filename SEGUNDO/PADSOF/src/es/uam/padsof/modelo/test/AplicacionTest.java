package es.uam.padsof.modelo.test;

import static org.junit.Assert.*;

import es.uam.eps.sadp.grants.InvalidIDException;
import es.uam.eps.sadp.grants.InvalidRequestException;
import es.uam.padsof.modelo.excepcion.ColectivoExcepcion;
import es.uam.padsof.modelo.excepcion.ProyectoExcepcion;
import es.uam.padsof.modelo.excepcion.UsuarioExcepcion;
import es.uam.padsof.modelo.proyecto.*;
import org.junit.Before;
import org.junit.Test;

import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.AplicacionExcepcion;
import es.uam.padsof.modelo.fechasimulada.FechaSimulada;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;


/**
 * Clase que funciona como test de la clase aplicacion
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public class AplicacionTest {
	private Aplicacion aplicacion;
	private ArrayList<UsuarioRegistrado> usuarios;
	private ArrayList<Colectivo> colectivos;
	private ArrayList<Proyecto> proyectos;

    /**
     * Inicializacion de la aplicacion de manera basica, asi como inicializacion de los objetos que seran utilizados en el test
     * @throws IOException Esta excepcion nunca se lanzaran ya que no se cargara la aplicacion de ningun archivo
     * @throws ClassNotFoundException Esta excepcion nunca se lanzaran ya que no se cargara la aplicacion de ningun archivo
     */
	@Before
	public void setup() throws IOException, ClassNotFoundException {
	    // Borramos primero el fichero de la aplicacion serializado si existe de antes
        Files.deleteIfExists(Paths.get("apc.ser"));

        // Inicializamos la aplicacion
		aplicacion = new Aplicacion();
        usuarios = new ArrayList<>();
        colectivos = new ArrayList<>();
        proyectos = new ArrayList<>();
        Ambito ambito = Ambito.INTERNACIONAL;
        ArrayList<Distrito> distritos = new ArrayList<>();
        distritos.add(Distrito.Barajas);
        distritos.add(Distrito.Carabanchel);

        // Registramos varios usuarios en la aplicacion
        usuarios.add(new UsuarioRegistrado("04290368V", "1234", "Tom"));
		usuarios.add(new UsuarioRegistrado("01160720W", "1234", "AnaMaria"));
        usuarios.add(new UsuarioRegistrado("23933652J", "asdasd", "Sergio"));
        aplicacion.setUsuariosRegistrados(usuarios);

		// Creamos varios proyectos en la aplicacion
        proyectos.add(new ProyectoSocial("Proyecto 1", "Este es el proyecto 1", 25, usuarios.get(0), FechaSimulada.getHoy(), "Este es el grupo social del proyecto 1", ambito));
        proyectos.add(new ProyectoSocial("Proyecto 2", "Este es el proyecto 2", 25, usuarios.get(1), FechaSimulada.getHoy(), "Este es el grupo social del proyecto 2", ambito));
        proyectos.add(new Infraestructura("Proyecto 3", "Este es el proyecto 3", 25, usuarios.get(0), FechaSimulada.getHoy(), distritos, null));
        proyectos.add(new Infraestructura("Proyecto 4", "Este es el proyecto 4", 25, usuarios.get(1), FechaSimulada.getHoy(), distritos, null));
        proyectos.add(new ProyectoSocial("Proyecto 1", "Este es el proyecto 1", 25, usuarios.get(0), FechaSimulada.getHoy(), "Este es el grupo social del proyecto 1", ambito));
        proyectos.add(new ProyectoSocial("Proyecto 2", "Este es el proyecto 2", 25, usuarios.get(1), FechaSimulada.getHoy(), "Este es el grupo social del proyecto 2", ambito));
        proyectos.add(new Infraestructura("Proyecto 3", "Este es el proyecto 3", 25, usuarios.get(0), FechaSimulada.getHoy(), distritos, null));
        proyectos.add(new Infraestructura("Proyecto 4", "Este es el proyecto 4", 25, usuarios.get(1), FechaSimulada.getHoy(), distritos, null));
        aplicacion.setProyectos(proyectos);

        // Creamos varios colectivos en la aplicacion
        colectivos.add(new Colectivo("Colectivo 1", usuarios.get(0)));
        colectivos.add(new Colectivo("Colectivo 2", usuarios.get(1)));
        aplicacion.setColectivos(colectivos);
	}

    /**
     * Tester de login sin ningun usuario logueado
     */
	@Test
	public void login1(){
		assertEquals(true, aplicacion.login("Tom", "1234"));
		assertEquals(aplicacion.getUsuarioActual(), usuarios.get(0));
	}

    /**
     * Tester de login de administrador
     */
    @Test
    public void login3(){
        assertEquals(true, aplicacion.login("admin", "admin"));
        assertNotNull(aplicacion.getAdmin());
    }

    /**
     * Tester de logout con usuario con sesion inciada
     */
    @Test
    public void logout1(){
        aplicacion.login("Tom", "1234");
        aplicacion.logout();
        assertNull(aplicacion.getUsuarioActual());
    }

    /**
     * Tester de logout con admin con sesion iniciada
     */
    @Test
    public void logout2(){
        aplicacion.login("admin", "admin");
        aplicacion.logout();
        assertNull(aplicacion.getAdmin());
    }

    /**
     * Tester de registro de un usuario con parametros correctos en la aplicacion
     */
    @Test
    public void registrar1(){
        Exception excepcion = null;
        try {
            aplicacion.registrar("Usuario1", "29866754N", "1234");
        } catch (UsuarioExcepcion usuarioExcepcion) {
            excepcion = usuarioExcepcion;
        }

        assertNull(excepcion);
    }

    /**
     * Tester de registro con nombre ya registrado
     */
    @Test
    public void registrar3(){
        Exception excepcion = null;
        try {
            aplicacion.registrar("Tom", "29866754N", "1234");
        } catch (UsuarioExcepcion usuarioExcepcion) {
            excepcion = usuarioExcepcion;
        }

        assertNotNull(excepcion);
    }

    /**
     * Tester de creacion de colectivo correctamente
     */
    @Test
    public void crearColectivo1(){
        Exception excepcion = null;
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.crearColectivo("Ejemplo 1");
        } catch (ColectivoExcepcion e) {
            excepcion = e;
        }
        assertNull(excepcion);
    }

    /**
     * Tester de creacion de colectivo con nombre ya utilizado
     */
    @Test
    public void crearColectivo2(){
        Exception excepcion = null;
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.crearColectivo("Colectivo 1");
        } catch (ColectivoExcepcion e) {
            excepcion = e;
        }
        assertNotNull(excepcion);
    }

    /**
     * Tester de creacion de colectivo con usuario bloqueado
     */
    @Test
    public void crearColectivo3(){
        usuarios.get(0).bloqueado();
        Exception excepcion = null;
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.crearColectivo("Ejemplo 2");
        } catch (ColectivoExcepcion e) {
            excepcion = e;
        }
        assertNotNull(excepcion);
    }

    /**
     * Tester de crear colectivo hijo de manera correcta
     */
    @Test
    public void crearColectivoHijo1(){
        Exception excepcion = null;
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.crearColectivoHijo("Colectivo hijo", colectivos.get(0));
        } catch (AplicacionExcepcion | ColectivoExcepcion e) {
            excepcion = e;
        }
        assertNull(excepcion);
    }

    /**
     * Tester de crear coectivo hijo con nombre ya utilizado
     */
    @Test
    public void crearColectivoHijo2(){
        Exception excepcion = null;
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.crearColectivoHijo("Colectivo 2", colectivos.get(0));
        } catch (AplicacionExcepcion | ColectivoExcepcion e) {
            excepcion = e;
        }
        assertNotNull(excepcion);
    }

    /**
     * Tester de crear proyecto social con usuario de manera correcta
     */
    @Test
    public void crearProyectoSocial1(){
        Exception excepcion = null;
        Ambito ambito = Ambito.INTERNACIONAL;
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.crearProyectoSocial("Proyecto 3", "Este es el proyecto 3", 25, "Este es el grupo social del proyecto 3", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion e) {
            excepcion = e;
        }
        assertNull(excepcion);
    }

    /**
     * Tester de crear proyecto social con colectivo de manera correcta
     */
    @Test
    public void crearProyectoSocial2(){
        Exception excepcion = null;
        Ambito ambito = Ambito.INTERNACIONAL;
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.crearProyectoSocial("Proyecto 3", "Este es el proyecto 3", 25, colectivos.get(0), "Este es el grupo social del proyecto 3", ambito, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion e) {
            excepcion = e;
        }
        assertNull(excepcion);
    }

    /**
     * Tester de crear proyecto infraestructura con usuario de manera correcta
     */
    @Test
    public void crearProyectoInfraestructura1(){
        Exception excepcion = null;
        ArrayList<Distrito> distritos = new ArrayList<>();
        distritos.add(Distrito.Barajas);
        distritos.add(Distrito.Carabanchel);
        aplicacion.login("Tom", "1234");
        File fichero =  new File("ejemplo.jpg");
        try {
            aplicacion.crearProyectoInfraestructura("Proyecto 3", "Este es el proyecto 3", 25, distritos, fichero , FechaSimulada.getHoy());
        } catch (ProyectoExcepcion e) {
            excepcion = e;
        }
        assertNull(excepcion);
    }

    /**
     * Tester de crear proyecto infraestructura como colectivo de manera correcta
     */
    @Test
    public void crearProyectoInfraestructura3(){
        Exception excepcion = null;
        ArrayList<Distrito> distritos = new ArrayList<>();
        distritos.add(Distrito.Barajas);
        distritos.add(Distrito.Carabanchel);
        aplicacion.login("Tom", "1234");
        File fichero =  new File("ejemplo.jpg");
        try {
            aplicacion.crearProyectoInfraestructura("Proyecto 3", "Este es el proyecto 3", 25, colectivos.get(0), distritos, fichero, FechaSimulada.getHoy());
        } catch (ProyectoExcepcion e) {
            excepcion = e;
        }
        assertNull(excepcion);
    }

    /**
     * Tester de crear proyecto infraestructura con usuario no representante del colectivo
     */
    @Test
    public void crearProyectoInfraestructura4(){
        Exception excepcion = null;
        ArrayList<Distrito> distritos = new ArrayList<>();
        distritos.add(Distrito.Barajas);
        distritos.add(Distrito.Carabanchel);
        aplicacion.login("Sergio", "asdasd");
        File fichero =  new File("ejemplo.jpg");
        try {
            aplicacion.crearProyectoInfraestructura("Proyecto 3", "Este es el proyecto 3", 25, colectivos.get(0), distritos, fichero , FechaSimulada.getHoy());
        } catch (ProyectoExcepcion e) {
            excepcion = e;
        }
        assertNotNull(excepcion);
    }


    /**
     * Tester que comprueba que votar proyecto como usuario funciona correctamente
     */
    @Test
    public void votarProyecto1(){
        Exception excepcion = null;
        aplicacion.login("Sergio", "asdasd");
        Proyecto proyecto = proyectos.get(0);
        try {
            aplicacion.votarProyecto(proyecto);
        } catch (AplicacionExcepcion e) {
            excepcion = e;
        }
        assertNull(excepcion);
        assertEquals(proyecto.getVotos(), 2);
    }

    /**
     * Tester que comprueba que votar proyecto como colectivo funciona correctamente
     */
    @Test
    public void votarProyecto2(){
        Exception excepcion = null;
        Colectivo colectivo = colectivos.get(0);
        colectivo.seguirColectivo(usuarios.get(1));
        colectivo.seguirColectivo(usuarios.get(2));
        aplicacion.login("Tom", "1234");
        try {
            aplicacion.votarProyecto(colectivo, proyectos.get(0));
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            excepcion = aplicacionExcepcion;
        }
        assertNull(excepcion);
        assertEquals(proyectos.get(0).getVotos(), 3);
    }

    /**
     * Tester que comprueba que solo el representante del colectivo puede votar un proyecto como colectivo
     */
    @Test
    public void votarProyecto3(){
        Exception excepcion = null;
        Colectivo colectivo = colectivos.get(0);
        Proyecto proyecto = proyectos.get(1);
        aplicacion.login("Sergio", "asdasd");
        try {
            aplicacion.votarProyecto(colectivo, proyecto);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            excepcion = aplicacionExcepcion;
        }
        assertNotNull(excepcion);
    }

    /**
     * Tester que comprueba si los proyectos caducan pasados 30 dias
     */
    @Test
    public void comprobarCaducados(){
        int tam = proyectos.size();
        FechaSimulada.avanzar(31);
        aplicacion.comprobarCaducados();
        assertEquals(tam,aplicacion.getProyectosCaducados().size());
    }

    /**
     * Tester que comprueba si los proyectos han pasado el umbral de votos
     */
    @Test
    public void comprobarVotos1(){
        Exception excepcion = null;
        try {
            aplicacion.setNumero_votos(0);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            System.out.println(aplicacionExcepcion);
        }
        aplicacion.login("Sergio", "asdasd");
        Proyecto proyecto = proyectos.get(0);
        try {
            aplicacion.votarProyecto(proyecto);
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            excepcion = aplicacionExcepcion;
        }
        assertNull(excepcion);
        aplicacion.comprobarVotos();
        assertEquals(aplicacion.getProyectosSolicitable().size(), 0);
    }

    /**
     * Tester que comprueba si los proyectos que han pasado el umbral de votos vuelven a pasar a estado a aceptado
     * si no superan el nuevo umbral
     */
    @Test
    public void comprobarVotos2(){
        aplicacion.setVotos(5);
        Proyecto proyecto = proyectos.get(0);
        proyecto.solicitable();
        aplicacion.comprobarVotos();
        assertEquals(proyecto.isAceptado(), true);
    }

    /**
     * Tester que comprueba si el metodo que manda un proyecto a solicitar financiacion funciona correctamente
     * Este test puede fallar debido a un fallo de conexion con
     */
    @Test
    public void solicitarFinanciacion(){
        Exception excepcion = null;
        UsuarioRegistrado usuario = usuarios.get(0);
        Proyecto proyecto = proyectos.get(0);
        proyecto.solicitable();
        aplicacion.setUsuarioActual(usuario);
        try {
            aplicacion.solicitarFinanciacion(proyecto);
        } catch (AplicacionExcepcion | InvalidRequestException e) {
            excepcion = e;
        } catch (IOException ignored){

        }
        assertNull(excepcion);
    }

    /**
     * Tester de bloqueo de usuario registrado
     */
    @Test
	public void bloquearUsuario(){
        Exception excepcion = null;
        aplicacion.login("admin", "admin");
        try {
            aplicacion.bloquearUsuario(usuarios.get(0));
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            excepcion = aplicacionExcepcion;
        }
        assertEquals(true, usuarios.get(0).isBloqueado());
        assertNull(excepcion);
	}

    /**
     * Tester de desbloqueo de usuario registrado
     */
	@Test
	public void desbloquearUsuario(){
	    Exception excepcion = null;
        aplicacion.login("admin", "admin");
        try {
            aplicacion.bloquearUsuario(usuarios.get(0));
            aplicacion.desbloquearUsuario(usuarios.get(0));
        } catch (AplicacionExcepcion aplicacionExcepcion) {
            excepcion = aplicacionExcepcion;
        }
        assertNull(excepcion);
        assertEquals(false, usuarios.get(0).isBloqueado());
	}

    /**
     * Tester que comprueba si el metodo de limpiar notificaciones funciona correctamente
     */
	@Test
	public void limpiarNotificacionesUsuario(){
	    Exception excepcion = null;
		ArrayList<String> notificaciones = new ArrayList<>();
		notificaciones.add("1");
		notificaciones.add("2");
		UsuarioRegistrado usuario = usuarios.get(0);
		usuario.setNotificaciones(notificaciones);
		aplicacion.login("Tom", "1234");
        aplicacion.limpiarNotificacionesUsuario();
        assertNull(excepcion);
        assertEquals(0, aplicacion.getNotificaciones().size());
    }

}
