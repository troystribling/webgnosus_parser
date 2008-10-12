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

%% webgnosus dictionary
-record(webgnosus_dictionary, 
    {
        word,
        language
    }
).

%% webgnosus dictionary
-record(webgnosus_punctuation, 
    {
        word,
        regexp,
        type
    }
).
