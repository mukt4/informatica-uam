package es.uam.padsof.modelo.usuario.registrado;

/**
 * Clase EstadoUsuario de tipo enumeracion
 * BLOQUEADO
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public enum EstadoUsuario {
    /**
     * ACEPTADO Estado en el que el administrador ha aceptado al usuario en la aplicacion
     */
    ACEPTADO{
        public String toString() {
            return "Aceptado";
        }
    },
    /**
     * BLOQUEADO Estado en el que el adminstrador ha bloqueado al usuario en la aplicacion
     */
    BLOQUEADO{
        public String toString() {
            return "Bloqueado";
        }
    }
}
