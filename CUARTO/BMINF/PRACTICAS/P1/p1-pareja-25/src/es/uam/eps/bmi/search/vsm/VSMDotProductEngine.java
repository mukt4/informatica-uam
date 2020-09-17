package es.uam.eps.bmi.search.vsm;

import java.io.IOException;

import es.uam.eps.bmi.search.AbstractEngine;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRanking;

public class VSMDotProductEngine extends AbstractEngine{

	public VSMDotProductEngine(Index idx) {
		super(idx);
		// TODO Auto-generated constructor stub
	}

	@Override
	public SearchRanking search(String query, int cutoff) throws IOException {
		// TODO Auto-generated method stub
		return null;
	}

}
