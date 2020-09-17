package es.uam.eps.bmi.sna.metric.user;

import es.uam.eps.bmi.sna.metric.LocalMetric;
import es.uam.eps.bmi.sna.ranking.Ranking;
import es.uam.eps.bmi.sna.ranking.RankingImpl;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetwork;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class UserClusteringCoefficient<U extends Comparable<U>> implements LocalMetric<U, U> {
    private int topk = -1;

    public UserClusteringCoefficient(int topk){
        this.topk = topk;
    }

    public UserClusteringCoefficient(){

    }

    @Override
    public Ranking<U> compute(UndirectedSocialNetwork<U> network) {
        Ranking<U> ranking;

        if(topk == -1){
            ranking = new RankingImpl<>();
        }
        else{
            ranking = new RankingImpl<>(topk);
        }

        for(U usuario : network.getUsers()){
            ranking.add(usuario, compute(network, usuario));
        }
        return ranking;
    }

    @Override
    public double compute(UndirectedSocialNetwork<U> network, U element) {
        int n_conexiones = 0;
        double posibles_conexiones = (network.getContacts(element).size() * (network.getContacts(element).size() - 1)) / 2;

        for(U usuario : network.getContacts(element)){
            for(U usuario1 : network.getContacts(element)){
                if(network.connected(usuario, usuario1)){
                    n_conexiones++;
                }
            }
        }

        // Dividimos entre dos al final para eliminar conexiones repetidas
        return n_conexiones / (posibles_conexiones * 2);
    }
}
