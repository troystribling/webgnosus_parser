require 'rake'
require 'rake/clean'

# Configuration
START_MODULE = "webgnosus"
MNESIA_DIR   = "/home/troy/mnesia/webgnosus"


# No Need to change
PWD = `pwd`.strip
INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/**/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CLEAN.include(['**/*.dump'])
CLOBBER.include(['**/*.beam'])

directory 'ebin'

rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

desc "compile all"
task :compile => ['ebin'] + OBJ

desc "open up a shell"
task :shell => [:compile] do
    sh("erl -boot start_sasl -sname #{START_MODULE} -pa #{PWD}/ebin  #{PWD}/src -run #{START_MODULE} init_shell -mnesia dir '\"#{MNESIA_DIR}\"'")
end

desc "open up a shell and start collector" 
task :collect => [:compile] do
    sh("erl -boot start_sasl -config src/#{START_MODULE} -sname #{START_MODULE} -pa #{PWD}/ebin #{PWD}/src -run #{START_MODULE} start -mnesia dir '\"#{MNESIA_DIR}\"'")
end

desc "open up a shell and run #{START_MODULE}:create_tables()" 
task :create_tables => [:compile] do
    sh("erl -boot start_sasl -sname #{START_MODULE} -pa #{PWD}/ebin #{PWD}/src -run #{START_MODULE} create_tables -mnesia dir '\"#{MNESIA_DIR}\"'")
end

desc "open up a shell and run #{START_MODULE}:delete_tables()" 
task :delete_tables => [:compile] do
    sh("erl -boot start_sasl -sname #{START_MODULE} -pa #{PWD}/ebin #{PWD}/src -run #{START_MODULE} delete_tables -mnesia dir '\"#{MNESIA_DIR}\"'")
end

desc "open up a shell and run #{START_MODULE}:delete_tables()" 
task :clear_tables => [:compile] do
    sh("erl -boot start_sasl -sname #{START_MODULE} -pa #{PWD}/ebin #{PWD}/src -run #{START_MODULE} clear_tables -mnesia dir '\"#{MNESIA_DIR}\"'")
end

desc "generate Documentation"
task :doc do
    sh("cd doc && erl -noshell -run edoc files ../#{SRC.join(" ../")} -run init stop")
end

task :default => :compile




