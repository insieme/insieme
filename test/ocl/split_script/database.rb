require "rubygems"
require "sequel"

$file_name = 'simple'

DB = Sequel.sqlite('simple.db')
$dynamic = DB[:dynamic_features]
$setup = DB[:setup]

def write_dynamic_features
  names = ['splittable_read_transfer',
	'splittable_write_transfer',
	'unsplittable_read_transfer',
	'unsplittable_write_transfer',
	'size'	]

  # generate the dynamic features name if not in the DB
  names.each{ |n| 
    if ($dynamic.filter(:name => n).count == 0)
      $dynamic.insert(:name => n)
    end
  }

  # read the dynamic features from the file x.dynamic
  values = `cat #{$file_name}.dynamic | awk -v x=2 '{print $x }'`.split # read values from file
  # FIXME: I have to do some operation on the dynamic features based on the size
  values[4] = 60.to_s #size

  # insert the dynamic features values in the 'setup' table
  $setup.select(:sid).count == 0 ? sid = 1 : sid = $setup.select(:sid).order(:sid).last[:sid] + 1
  
  names.zip(values).each do |name, value|
    fid =  $dynamic.filter(:name => name).select(:id).single_value
    #puts "#{sid} #{fid} #{value}"
    $setup.insert(:sid => sid, :fid => fid, :value => value)
  end


end

write_dynamic_features

