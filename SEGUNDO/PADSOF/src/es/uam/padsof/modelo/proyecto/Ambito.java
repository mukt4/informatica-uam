package es.uam.padsof.modelo.proyecto;

/**
 * Clase Ambito de tipo enumeracion
 * NACIONAL O INTERNACIONAL
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public enum  Ambito {
    /**
     * NACIONAL para representar proyectos nacionales
     */
    NACIONAL
            {
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor NACIONAL
         */
        public String toString() {
            return "Nacional";
        }
    },
    /**
     * INTERNACIONAL para representar proyectos internacionales
     */
    INTERNACIONAL{
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor INTERNACIONAL
         */
        public String toString() {
            return "Internacional";
        }
    }
}
