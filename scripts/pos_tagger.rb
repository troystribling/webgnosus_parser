#### read specied file and apply block to row
def read_file (file)
    File.open(file, "r") do |f|
        while row = f.gets
            yield row
        end
    end
end

#### run tagger and return output
def run_tagger()
end

#### run program
read_file(ARGV.first) do |row|
  puts row
end