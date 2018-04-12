class UniversityProgram
  attr_accessor :program_name
  attr_accessor :scholarship_amount

  def initialize(name, amount)
    @program_name = name
    @scholarship_amount = amount
  end

  def course_points_per_year
    60
  end

  def has_scholarship?
    true
  end

  def requirement_for_scholarship
    0.75
  end

  def points_required_for_scholarship
    course_points_per_year * requirement_for_scholarship
  end

  def qualifies_for_scholarship?(student)
    has_scholarship? &&
      student.points_current_year >= points_required_for_scholarship
  end

  # Standard calculation, which awards the full amount for any qualifying
  # student
  def awarded_scholarship_amount(student)
    if qualifies_for_scholarship?(student)
      scholarship_amount
    else
      0
    end
  end
end

class FancyProgram < UniversityProgram
  # Scholarship awarded proportionally to the amount of course points
  def awarded_scholarship_amount(student)
    point_ratio = [1, student.points_current_year.to_f / course_points_per_year.to_f].min
    fulfillment = [0, point_ratio - requirement_for_scholarship].max
    scholarship_amount * fulfillment
  end
end

class BotanicalProgram < FancyProgram
  def requirement_for_scholarship
    0.9
  end
end

class Student
  attr_accessor :name
  attr_accessor :enrolled_in_program
  attr_accessor :points_current_year

  def initialize(name, prog, points)
    @name = name
    @enrolled_in_program = prog
    @points_current_year = points
  end

  def awarded_scholarship_amount
    enrolled_in_program.awarded_scholarship_amount(self)
  end
end

module TestValues
  def computer_science
    UniversityProgram.new("Computer Science", 10000)
  end

  def software_engineering
    UniversityProgram.new("Software Engineering", 20000)
  end

  def industrial_economy
    FancyProgram.new("Industrial Economy", 15000)
  end

  def plant_science
    BotanicalProgram.new("Plant Science", 25000)
  end

  def students
    [Student.new("Edvin", computer_science, 50),
     Student.new("Maria", software_engineering, 35),
     Student.new("Dolly", industrial_economy, 55),
     Student.new("Zorro", plant_science, 55)
    ]
  end
end
