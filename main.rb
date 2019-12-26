a, b, c, _ = (gets.chomp.split /\s/).map { |x| x.to_i }

s = gets.chomp
x = s.map do |c| c.to_i end

n = gets.chomp.to_i
y = []
(0...n).each do |i|
  y.push ((gets.chomp.split /\s/).map { |x| x.to_i })
end

exit
