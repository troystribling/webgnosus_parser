#### read specied file and apply block to row
def read_file (file)
    File.open(file, "r") do |f|
        while row = f.gets
            yield row
        end
        f.close()
    end
end

#### run program
out_file = File.open(ARGV.last, "w")
read_file(ARGV.first) do |row|
    puts "PROCESSING: #{row}"
    File.open("tmp_in.dat", "w+") {|f| f.write(row); f.close()}
    `java -mx500m -classpath stanford-postagger.jar edu.stanford.nlp.tagger.maxent.MaxentTagger -model models/bidirectional-wsj-0-18.tagger -prop myPropsFile.prop -textFile tmp_in.dat > tmp_out.dat`
    File.open("tmp_out.dat", "r") do |f| 
        tagged = f.readlines(row)
        f.close()
        puts "TAGGED: #{tagged}"
        out_file.write(tagged)
    end
end
out_file.close()