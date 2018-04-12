# Dynamic dispatch in Ruby

class C1
  def a
    b+1
  end
  def b
    100
  end
end

class C2 < C1
  def b
    200
  end
end

puts C1.new.a  # prints 101
puts C2.new.a  # prints 201
