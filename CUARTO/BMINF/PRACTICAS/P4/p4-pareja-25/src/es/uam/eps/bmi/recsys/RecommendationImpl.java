package es.uam.eps.bmi.recsys;

import es.uam.eps.bmi.recsys.ranking.Ranking;
import es.uam.eps.bmi.recsys.ranking.RankingElement;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class RecommendationImpl implements Recommendation {
    private Map<Integer, Ranking> userRankingMap;

    public RecommendationImpl(){
        userRankingMap = new HashMap<>();
    }

    @Override
    public Set<Integer> getUsers() {
        return userRankingMap.keySet();
    }

    @Override
    public Ranking getRecommendation(int user) {
        return userRankingMap.get(user);
    }

    @Override
    public void add(int user, Ranking ranking) {
        userRankingMap.put(user, ranking);
    }

    @Override
    public void print(PrintStream out) {
        for (Map.Entry<Integer, Ranking> entryU : userRankingMap.entrySet()) {
            for (RankingElement r : entryU.getValue())
                out.println("\t" + r.getID() + "\t" + r.getScore());
        }
    }

    @Override
    public void print(PrintStream out, int userCutoff, int itemCutoff) {
        int uCount = 0;

        for (Map.Entry<Integer, Ranking> entryU : userRankingMap.entrySet()) {
            if (uCount < userCutoff){
                int iCount = 0;

                for (Iterator<RankingElement> itemIterator = getRecommendation(entryU.getKey()).iterator();
                     itemIterator.hasNext() && iCount < itemCutoff; iCount++) {

                    out.append(entryU.getKey().toString());
                    RankingElement r = itemIterator.next();
                    out.println("\t" + r.getID() + "\t" + r.getScore());
                }
                uCount++;
            }
            else
                break;
        }
    }
}
