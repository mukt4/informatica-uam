package es.uam.eps.bmi.search.index.lucene;

import es.uam.eps.bmi.search.index.AbstractIndexBuilder;
import es.uam.eps.bmi.search.index.Index;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.lucene.analysis.CharArraySet;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.FieldType;
import org.apache.lucene.document.StringField;
import org.apache.lucene.index.IndexOptions;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.FSDirectory;

/**
 *
 * @author pablo
 */
public class LuceneBuilder extends AbstractIndexBuilder {
    IndexWriter builder;
    protected static FieldType type;
    String indexFolder;

    public LuceneBuilder() {
        type = new FieldType();
        type.setIndexOptions (IndexOptions.DOCS_AND_FREQS);
    }

    public void build (String collectionPath, String path) throws IOException {
        init(path);
        indexDocuments(collectionPath);
        close(path);
    }

    public void init(String indexPath) throws IOException {
        indexFolder = indexPath;
        clear(indexPath);
        IndexWriterConfig iwc = new IndexWriterConfig(new StandardAnalyzer(CharArraySet.EMPTY_SET));
        iwc.setOpenMode(IndexWriterConfig.OpenMode.CREATE);
        builder = new IndexWriter(FSDirectory.open(Paths.get(indexPath)), iwc);
    }

    public void close(String indexPath) throws IOException {
        builder.close();
        saveDocNorms(indexPath);
    }

    public void indexText(String text, String path) throws IOException {
        Document doc = new Document();
        Field pathField = new StringField("path", path, Field.Store.YES);
        doc.add(pathField);
        Field field = new Field("content", text, type);
        doc.add(field); 
        builder.addDocument(doc);
    }

    protected Index getCoreIndex() throws IOException {
        return new LuceneIndex(indexFolder);
    }
}
