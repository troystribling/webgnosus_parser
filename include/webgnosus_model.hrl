%% webgnosus information model

%% webgnosus words
-record(webgnosus_words, 
    {
        word,
        word_count,
        word_frequency,
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
