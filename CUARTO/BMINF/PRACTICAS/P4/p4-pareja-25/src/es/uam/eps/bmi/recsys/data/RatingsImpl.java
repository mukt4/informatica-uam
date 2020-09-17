package es.uam.eps.bmi.recsys.data;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class RatingsImpl implements Ratings {
    private Set<Integer> userSet;   // Set que guarda los userID
    private Map<Integer, Set<Integer>> itemMap;   // Mapa que guarda los itemsID - set usersID
    private Map<Integer, Map<Integer,Double>> ratingsMap; // Mapa UserID - (Mapa itemID - rating)
    private int nRatings;

    public RatingsImpl(){
        userSet = new HashSet<>();
        itemMap = new HashMap<>();
        ratingsMap = new HashMap<>();
        nRatings = 0;
    }

    public RatingsImpl(String ratingsFile, String separator) {

        userSet = new HashSet<>();
        itemMap = new HashMap<>();
        ratingsMap = new HashMap<>();
        nRatings = 0;

        try {
            FileReader fr = new FileReader(new File(ratingsFile));
            BufferedReader br = new BufferedReader(fr);

            String line;
            while((line = br.readLine()) != null){

                String[] items = line.split(separator); //[0]: Usuario, [1]: Item, [2]: Score
                rate(   Integer.parseInt(items[0]),
                        Integer.parseInt(items[1]),
                        Double.parseDouble(items[2]));

            }
        }
        catch(IOException e){
            System.out.print("!> ERROR: No se ha podido abrir el fichero de ratings.");
            e.printStackTrace();
        }
    }

    @Override
    public void rate(int user, int item, Double rating) {

        if (!this.userSet.contains(user)){   // Usuario NO registrado

            this.userSet.add(user);
        }

        if (!this.itemMap.containsKey(item)){    // Item NO registrado

            this.itemMap.put(item, new HashSet<>());
        }

        if (!this.itemMap.get(item).contains(user)){ // Usuario hace rate al item por primera vez

            Set<Integer> s = this.itemMap.get(item);
            s.add(user);
            this.itemMap.put(item, s);
        }

        if (!this.ratingsMap.containsKey(user)){

            this.ratingsMap.put(user, new HashMap<>());
        }

        Map<Integer,Double> m = this.ratingsMap.get(user);
        m.put(item, rating);
        this.ratingsMap.put(user, m);
        this.nRatings++;
    }

    @Override
    public Double getRating(int user, int item) {

        if (this.ratingsMap.get(user) != null)
            return this.ratingsMap.get(user).get(item);
        else
            return null;
    }

    @Override
    public Set<Integer> getUsers(int item) {

        return this.itemMap.get(item);
    }

    @Override
    public Set<Integer> getItems(int user) {

        if (ratingsMap.get(user) != null)
            return ratingsMap.get(user).keySet();
        else
            return null;
    }

    @Override
    public Set<Integer> getUsers() {

        return this.userSet;
    }

    @Override
    public Set<Integer> getItems() {

        return this.itemMap.keySet();
    }

    @Override
    public int nRatings() {

        return nRatings;
    }

    @Override
    public Ratings[] randomSplit(double ratio) {

        Ratings[] ratings = new Ratings[2];
        ratings[0] = new RatingsImpl();
        ratings[1] = new RatingsImpl();
        Random random = new Random();

        for (Map.Entry<Integer, Map<Integer,Double>> entryR : ratingsMap.entrySet()) {

            for (Map.Entry<Integer,Double> entryU : entryR.getValue().entrySet()) {

                if ((random.nextFloat() * (1.0f - 0.0f) + 0.0f) <= ratio)
                    ratings[0].rate(entryR.getKey(), entryU.getKey(), entryU.getValue());

                else
                    ratings[1].rate(entryR.getKey(), entryU.getKey(), entryU.getValue());

            }
        }
        return ratings;
    }

}
