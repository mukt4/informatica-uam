
/**
* Esta aplicacion calcula el numero de combinaciones sin repeticion de n elementos
* tomados de k en k.
* <p>La implementacion es recursiva, basada en c(n, k) = c(n-1, k-1) + c(n-1, k)
* Los casos base son c(n, 0) = 1 = c(n, n) y c(n, k) = 0 para todo k mayor que n</p>
* <p><b>Nota</b>: Esta implementacion no es muy eficiente, al hacer muchos calculos
redundantes.
* Se aconseja usar valores pequenios de n y k, entre 0 y 30</p>
*
* @author Tomas Higuera y Guillermo Hoyo tomas.higuera@estudiante.uam.es guillermo.hoyo@estudiante.uam.es
*
*/
public class Combinatoria {
	
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
	* Ejemplo de constructor, en esta clase serIa innecesario, ya que no tiene argumentos
	* ni inicializa atributos. El compilador crea uno igual si no existe.
	* Es importante que no devuelva nada (tampoco void), y que se llame como la clase.
	* Si fuese privado impediria crear objetos de este tipo desde otras clases.
	*/
	public Combinatoria(/* Argumentos para construir el objeto, si los hubiera */) {
	/* Esta clase no tiene atributos, por lo que este constructor vacio lo crearia
	* automaticamente el compilado, y no es necesario
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
		else if (k == 0 || n==k ) 
			return 1; //caso base para 1
		else if (k > n) 
			return 0; //caso base para 0
		//caso general
		else 
			return combinaciones(n-1, k-1)+ combinaciones (n-1, k);
	}
}

