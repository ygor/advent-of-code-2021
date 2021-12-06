
numbers = File.open("input.txt").readlines.first.strip.split(",").map(&:to_i)
fish = numbers.group_by(&:itself).to_h.transform_values(&:size)

def simulate (fish, days)
    (1 .. days).inject(fish) { |fish, i| 
        fish.inject({}) { |fish, (f, count)|  
            if f == 0 then { 6 => count, 8 => count}.inject(fish) { |h, (k, v)| h[k] = h.fetch(k, 0) + v; h }
            else fish[f - 1] = fish.fetch(f - 1, 0) + count; fish end }}
end

puts "Part 1: #{simulate(fish, 80).values.sum}"
puts "Part 2: #{simulate(fish, 256).values.sum}"