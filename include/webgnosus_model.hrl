%% webgnosus information model

%% webgnosus words
-record(webgnosus_words, 
    {
        word,
        count,
        frequency,
        document_count,
        document_frequency
    }
).
