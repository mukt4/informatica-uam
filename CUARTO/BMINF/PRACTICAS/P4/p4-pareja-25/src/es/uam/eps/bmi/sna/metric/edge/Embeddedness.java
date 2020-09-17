package es.uam.eps.bmi.sna.metric.edge;

import es.uam.eps.bmi.sna.metric.LocalMetric;
import es.uam.eps.bmi.sna.ranking.Ranking;
import es.uam.eps.bmi.sna.ranking.RankingImpl;
import es.uam.eps.bmi.sna.structure.Edge;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetwork;

import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class Embeddedness<U extends Comparable<U>> implements LocalMetric<Edge<U>, U> {
    private int topk;

    public Embeddedness(int topk){
        this.topk = topk;
    }

    @Override
    public Ranking<Edge<U>> compute(UndirectedSocialNetwork<U> network) {
        Ranking<Edge<U>> ranking = new RankingImpl<>(topk);
        Set<Edge<U>> anadidos = new HashSet<>();

        // Recorremos el listado de usuarios de la red
        for(U usuario1 : network.getUsers()){
            for(U usuario2 : network.getUsers()){
                if(!usuario1.equals(usuario2)){
                    Edge<U> edge = new Edge<>(usuario1, usuario2);
                    // Comprobamos si ya hemos anadido el edge
                    Edge<U> check = new Edge<>(usuario2, usuario1);
                    if(!anadidos.contains(check)) {
                        ranking.add(edge, compute(network, edge));
                        anadidos.add(edge);
                    }
                }
            }
        }
        return ranking;
    }

    @Override
    public double compute(UndirectedSocialNetwork<U> network, Edge<U> element) {
        double coincidentes = 0;

        U A = element.getFirst();
        U B = element.getSecond();

        // Obtenemos los amigos de los elementos de la arista
        Set<U> amigosA = new HashSet<>(network.getContacts(element.getFirst()));
        Set<U> amigosB = new HashSet<>(network.getContacts(element.getSecond()));

        // Eliminamos A y B de la lista de amigos del otro usuario
        amigosA.remove(B);
        amigosB.remove(A);

        for(U x : amigosA){
            for(U y : amigosB){
                if(x.equals(y)){
                    coincidentes++;
                }
            }
        }

        return coincidentes / (amigosA.size() + amigosB.size() - coincidentes);
    }
}
