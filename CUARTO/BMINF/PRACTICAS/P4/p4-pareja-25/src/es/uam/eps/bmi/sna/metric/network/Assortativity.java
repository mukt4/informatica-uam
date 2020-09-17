package es.uam.eps.bmi.sna.metric.network;

import es.uam.eps.bmi.sna.metric.GlobalMetric;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetwork;

import java.util.Map;
import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class Assortativity<U extends Comparable<U>> implements GlobalMetric<U> {
    @Override
    public double compute(UndirectedSocialNetwork<U> network) {
        // Obtenemos numero de aristas
        double m = network.nEdges();
        double grado_cuadrado = 0.0, grado_cubo = 0.0, grado = 0.0;

        for(U usuario : network.getUsers()){
            grado_cuadrado += Math.pow(network.getContacts(usuario).size(), 2);
            grado_cubo += Math.pow(network.getContacts(usuario).size(), 3);

            for(U usuario2 : network.getContacts(usuario)){
                grado += network.getContacts(usuario).size() * network.getContacts(usuario2).size();
            }
        }

        // Eliminamos arcos repetidos
        grado = grado / 2;
        grado_cuadrado = Math.pow(grado_cuadrado, 2);

        return (4.0 * m * grado - grado_cuadrado) / (2.0 * m * grado_cubo - grado_cuadrado);
    }
}
