package es.uam.eps.bmi.sna.metric.network;

import es.uam.eps.bmi.sna.metric.GlobalMetric;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetwork;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class ClusteringCoefficient<U extends Comparable<U>> implements GlobalMetric<U> {
    @Override
    public double compute(UndirectedSocialNetwork<U> network) {
        double contador = 0;
        double tamano;
        double total = 0;

        // Contamos el numero de caminos cerrados
        for (U usuario : network.getUsers()){
            for(U usuario1 : network.getContacts(usuario)){
                for(U usuario2 : network.getContacts(usuario1)){
                    if(network.connected(usuario, usuario2)){
                        contador++;
                    }
                }
            }
        }

        for(U usuario : network.getUsers()){
            tamano = network.getContacts(usuario).size();
            total += (tamano * (tamano - 1)) / 2;
        }

        // Dividimos entre dos para quitarnos las conexiones repetidas
        return contador/(total * 2);
    }
}
