
numbers = File.open("input.txt").readlines.first.strip.split(",").map(&:to_i)
initial_fish = numbers.group_by(&:itself).to_h.transform_values(&:size)

puts "#{{:part1 => 80, :part2 => 256 }.transform_values { |days|
    (1 .. days).inject(initial_fish) { |fish_on_day, i|
        fish_on_day.inject({}) { |fish, (f, count)|
            if f == 0 then { 6 => count, 8 => count}.inject(fish) { |h, (k, v)| h[k] = h.fetch(k, 0) + v; h }
            else fish[f - 1] = fish.fetch(f - 1, 0) + count; fish end }}.values.sum }}"