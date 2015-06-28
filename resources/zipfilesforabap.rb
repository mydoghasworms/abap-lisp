#!/usr/bin/ruby

# This script is used to generate the Base64-encoded zip file that is included inline in
# ZUSR_LISP_IDE

require 'zip'
require 'base64'

webfile_directory = 'webfiles'

files = ['ace.js', 'mode-scheme.js', 'theme-github.js', 'theme-eclipse.js']

zipfile_name = 'files.zip'
b64file_name = 'files.b64.txt'

# Delete existing output files if they exist
File.delete(zipfile_name) if File.exist?(zipfile_name)
File.delete(b64file_name) if File.exist?(b64file_name)

# Create zip file using given file list
Zip::File.open(zipfile_name, Zip::File::CREATE) do |zipfile|
  Dir::glob('./webfiles/*').each do |filename|
    puts "Adding #{filename} as #{File.basename(filename)}"
    zipfile.add(File.basename(filename), filename)
  end
end

# Encode zip file in Base64
# To save space in the resulting file, join every two lines
b64 = Base64.encode64(open(zipfile_name, 'rb').read)
joinline = false
output  = b64.split.reduce('') {|str, line|
  joinline = ! joinline
  str << '*' if joinline
  str << line
  str << "\n" unless joinline
  str
}

open(b64file_name, 'w') {|f|
  f.write(output)
}

