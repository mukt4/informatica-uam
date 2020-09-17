package Testers;

import IIndividuo.IIndividuo;
import IIndividuo.Individuo;
import INodo.Funcion.Funcion;
import INodo.Funcion.FuncionMultiplicacion;
import INodo.Funcion.FuncionResta;
import INodo.Funcion.FuncionSuma;
import INodo.Terminal.Terminal;
import INodo.Terminal.TerminalAritmetico;

public class TesterIndividuos {

    public static void main(String[] args) {
        Terminal x = new TerminalAritmetico("x");
        Funcion suma = new FuncionSuma("+", 2);
        Funcion resta = new FuncionResta("-", 2);
        Funcion multi = new FuncionMultiplicacion("*", 2);
        multi.incluirDescendiente(x);
        multi.incluirDescendiente(x);
        suma.incluirDescendiente(multi);
        suma.incluirDescendiente(x);
        resta.incluirDescendiente(suma);
        resta.incluirDescendiente(multi);
        System.out.println("Función multiplicación: " + multi);
        System.out.println();
        System.out.println("Función suma: " + suma);
        System.out.println();
        System.out.println("Función resta: " + resta);

        IIndividuo indiv = new Individuo();
        indiv.setExpresion(resta);
        System.out.println();
        System.out.println("INDIVIDUO");
        indiv.writeIndividuo();
    }
}
