%% webgnosus information model

%% webgnosus words
-record(webgnosus_words, 
    {
        word,
        count,
        frequency,
        pos
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
