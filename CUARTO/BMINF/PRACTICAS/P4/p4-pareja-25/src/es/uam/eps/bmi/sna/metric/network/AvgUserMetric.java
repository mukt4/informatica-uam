package es.uam.eps.bmi.sna.metric.network;

import es.uam.eps.bmi.sna.ranking.Ranking;
import es.uam.eps.bmi.sna.metric.GlobalMetric;
import es.uam.eps.bmi.sna.metric.user.UserClusteringCoefficient;
import es.uam.eps.bmi.sna.ranking.RankingElement;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetwork;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class AvgUserMetric <U extends Comparable<U>> implements GlobalMetric<U> {
    private UserClusteringCoefficient<U> userClusteringCoefficient;

    public AvgUserMetric(UserClusteringCoefficient<U> userClusteringCoefficient) {
        this.userClusteringCoefficient = userClusteringCoefficient;
    }

    @Override
    public double compute(UndirectedSocialNetwork<U> network) {
        Ranking<U> ranking = userClusteringCoefficient.compute(network);
        double average = 0;

        for (RankingElement<U> elemento : ranking) {
            average += elemento.getScore();
        }

        return average / ranking.size();
    }
}
