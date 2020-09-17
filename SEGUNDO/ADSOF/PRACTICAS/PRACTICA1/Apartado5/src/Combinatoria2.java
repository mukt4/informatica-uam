import java.util.*;

/**
* Esta aplicacion calcula el numero de combinaciones sin repeticion de n elementos
* tomados de k en k.
* <p>La implementacion es recursiva, basada en c(n, k) = c(n-1, k-1) + c(n-1, k)
* Los casos base son c(n, 0) = 1 = c(n, n) y c(n, k) = 0 para todo k mayor que n</p>
* <p><b>Nota</b>: Esta implementacion utiliza un HashMap para evitar repeticiones en los calculos
*
* @author Tomas Higuera y Guillermo Hoyo tomas.higuera@estudiante.uam.es guillermo.hoyo@estudiante.uam.es
*
*/
public class Combinatoria2 {
	
	/*
	* HashMap
	*/
	private Map<Integer, Long> cache = new HashMap<>();

	/*
	* Si la clase tuviera atributos, los declarariamos aqui, como
	* private Tipo1 atributo1;
	* private Tipo2 atributo2;
	* ...
	* Tambien se pueden inicializar al declararlos, por ejemplo
	* private int contador= 0;
	* El valor inicial tambien se puede asignar en el constructor
	*/

	/**
	* Ejemplo de constructor, en esta clase seria innecesario, ya que no tiene argumentos
	* ni inicializa atributos. El compilador crea uno igual si no existe.
	* Es importante que no devuelva nada (tampoco void), y que se llame como la clase.
	* Si fuese privado impediria crear objetos de este tipo desde otras clases.
	*/
	public Combinatoria2(/* Argumentos para construir el objeto, si los hubiera */) {
	/* Esta clase no tiene atributos, por lo que este constructor vacio lo crearia
	* automoticamente el compilado, y no es necesario
	* Si tenemos un atributo (atributo1) con el mismo nombre que un argumento, podemos
	* usar
	* "this.atributo1" para referirnos al atributo, y "atributo1" para el argumento
	* Por ejemplo podemos asignar el valor inicial con:
	* this.atributo1 = atributo1;
	*/
	}
	
	/**
	 * Devuelve el numero de combinaciones posibles de n elementos tomados de k en k
	 * @param n Numero de elementos totales	
	 * @param k Numero de elementos, sin repeticion, en cada combinacion
	 * @return valor del coeficiente binomial (n, k)
	 */
	public long combinaciones(int n, int k){
		//Primero comprobamos si los argumentos son validos
		if (n<0 || k <0) 
			throw new IllegalArgumentException("n y k han de ser positivos");
		//Casos base
		if(cache.containsKey(posicion(n, k))==true) {
			return cache.get(posicion(n, k));
		}
		else {
			if (k == 0 || n==k ) {
				cache.put(posicion(n, k),  (long) 1);
				return 1; //caso base para 1
			}
			else if (k > n) {
				cache.put(posicion(n, k),  (long) 0);
				return 0; //caso base para 0
			}
			//caso general
			else {
				cache.put(posicion(n, k),  (combinaciones(n-1, k-1)+ combinaciones (n-1, k)));
				return combinaciones(n-1, k-1)+ combinaciones (n-1, k);
			}
		}
	}
	/*
	* Clase privada que devuelve la posicion de la clave en el HashMap
	* @param n numero total de elementos
	* @param k clave a buscar
	* @return int posicion de la clave en el HashMap
	*/	
	private int posicion(int n, int k){
		/* ... implementacion ... */
		return n*(n+1)/2 + k;
	}
}

