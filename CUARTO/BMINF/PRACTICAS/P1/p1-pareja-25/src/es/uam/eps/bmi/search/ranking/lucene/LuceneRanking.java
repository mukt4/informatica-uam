package es.uam.eps.bmi.search.ranking.lucene;

import java.util.Iterator;

import org.apache.lucene.search.ScoreDoc;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;

public class LuceneRanking implements SearchRanking {

	public LuceneRanking(Index index, ScoreDoc[] scoreDocs) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public Iterator<SearchRankingDoc> iterator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int size() {
		// TODO Auto-generated method stub
		return 0;
	}

}
