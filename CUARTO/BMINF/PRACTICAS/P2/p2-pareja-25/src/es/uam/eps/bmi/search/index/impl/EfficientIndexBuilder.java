package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.AbstractIndexBuilder;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.IndexBuilder;

import java.io.IOException;

public class EfficientIndexBuilder extends AbstractIndexBuilder implements IndexBuilder{

    @Override
    protected void indexText(String text, String path) throws IOException {

    }

    @Override
    protected Index getCoreIndex() throws IOException {
        return null;
    }

    @Override
    public void build(String collectionPath, String indexPath) throws IOException {

    }
}
