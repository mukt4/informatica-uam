/**
* Esta aplicacion crea un triangulo de tartaglia de un numero determinado de elementos
*
* @author Tomas Higuera y Guillermo Hoyo tomas.higuera@estudiante.uam.es guillermo.hoyo@estudiante.uam.es
*
*/
public class Tartaglia {
	
	private Combinatoria c;
	private int n;
	
	/**
	 * Constructor del objeto Tartaglia
	 * @param c Numero de elementos totales	
	 * @param n Numero de elementos, sin repeticion, en cada combinacion
	 */
	public Tartaglia (Combinatoria c, int n) {
		this.c=c;
		this.n=n;
	}
	
	/**
	 * Metodo que devuelve el triangulo de Tartaglia en forma de String
	 * @return String Devuelve un string que contiene el triangulo de Tartaglia	
	 */
	public String toString() {
		String triangulo = new String();
		int i;
		int j;
		
		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j <= i; j++) {
				triangulo += c.combinaciones(i,j) + " ";
			}
			triangulo += "\n";
		}
		return triangulo;
	}
	
	/**
	 * Punto de entrada a la aplicacion.
	 *
	 * <p>Este metodo imprime el valor del coeficiente binomial de los 2 parametros de
	entrada</p>
	 *
	 * @param args Los argumentos de la linea de comando. Se esperan dos numeros enteros, como
	 * cadenas
	 */
	public static void main(String[] args) {
		if (args.length!=1) {
			System.out.println("Se espera un numero como parametro, n.");
			System.out.println(" n = Numero total de elementos ");
		}
		else {
			int n = Integer.parseInt(args[0]); // convertimos String a int
			Combinatoria c = new Combinatoria(); // Creamos un objeto c de tipo Combinatoria
			System.out.println(new Tartaglia(c, n)); // Imprimimos una linea por salida estandar
		}
	}
	
}
