package es.uam.padsof.modelo.proyecto;

/**
 * Clase EstadoProyecto de tipo enumeracion
 * @author Tomas Higuera Viso y Colman Lopez Alonso
 * @version 1.0
 */
public enum EstadoProyecto {
    /**
     *  PENDIENTE El proyecto se encuentra pendiente de ser aceptado por el administrador
     */
    PENDIENTE {
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor PENDIENTE
         */
        public String toString() {
            return "Pendiente";
        }
    },
    /**
     * SOLICTABLE El proyecto ha alcanzado el numero minimo de votos y puede solicitar financiaciacion
     */
    SOLICITABLE {
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor SOLICITABLE
         */
        public String toString(){
            return "Solicitable";
        }
    },
    /**
     * RECHAZADO El proyecto se encuentra rechazado por el administrador
     */
    RECHAZADO{
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor RECHAZADO
         */
        public String toString() {
            return "Rechazado";
        }
    },
    /**
     * CADUCADO El proyecto se encuentra caducado
     */
    CADUCADO{
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor CADUCADO
         */
        public String toString() {
            return "Caducado";
        }
    },
    /**
     * ACEPTADO El proyecto se encuentra aceptado por el administrador
     */
    ACEPTADO{
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor ACEPTADOL
         */
        public String toString() {
            return "Aceptado";
        }
    },
    /**
     * FINANCIADO El proyecto ha sido aceptado para financiacion
     */
    FINANCIADO{
        /**
         * Metodo toString para definir el valor de enum en forma de cadena
         * @return String que representa el valor FINANCIADO
         */
        public String toString() {
            return "Financiado";
        }
    },

    /**
     * ESPERA El proyecto se encuentra en espera de financiacion
     */
    ESPERA{
        public String toString() {
            return "Espera";
        }
    }

}
