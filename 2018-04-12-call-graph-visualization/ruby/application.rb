# Run as follows:
#
#     ruby -Iruby ruby/application.rb

require 'scholarship'

include TestValues

students.each do |student|
  printf("Scholarship for %s: %d\n", student.name, student.awarded_scholarship_amount)
end
