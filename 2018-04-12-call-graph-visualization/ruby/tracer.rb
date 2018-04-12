# Run as follows:
#
#     ruby -Iruby ruby/tracer.rb > trace.log

require 'scholarship'

include TestValues

trace = []

students.each do |student|
  trace << ("#### " + student.enrolled_in_program.class.name)
  set_trace_func proc { |event, _, line, method, _, classname|
    if (event == "call" || event == "return")
      trace << sprintf("%s,%d,%s,%s", event, line, method, classname)
    end
  }
  student.awarded_scholarship_amount
  set_trace_func(nil)
end

puts trace.join("\n")
