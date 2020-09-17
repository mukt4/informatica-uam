package es.uam.eps.bmi.search.lucene;

import java.io.IOException;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.queryparser.classic.ParseException;

import es.uam.eps.bmi.search.AbstractEngine;
import es.uam.eps.bmi.search.index.lucene.LuceneIndex;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.lucene.LuceneRanking;

public class LuceneEngine extends AbstractEngine{

	
	private IndexSearcher indexSearcher_LuceneE;
	private QueryParser queryParser_LuceneE;
	
	public LuceneEngine(String idx) throws IOException {
		
		super(new LuceneIndex(idx));
		//Stopwords incluidos
		queryParser_LuceneE = new QueryParser("contenido",new StandardAnalyzer());
		indexSearcher_LuceneE = new IndexSearcher(((LuceneIndex) this.index).getIReader());
	}
	@Override
	public SearchRanking search(String query, int cutoff) throws IOException {
		
		TopDocs tpDocs;
		
		if(null == this.index) throw new IOException();
		
		try {
			tpDocs = indexSearcher_LuceneE.search(queryParser_LuceneE.parse(query),cutoff);
		} catch (ParseException pe) {
			pe.printStackTrace();
			return null;
		}
		return new LuceneRanking(this.index,tpDocs.scoreDocs);
	}

}
