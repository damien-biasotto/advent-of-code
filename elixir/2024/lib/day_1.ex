defmodule Aoc2024.Day1 do
@doc """
##Part 1
iex> input = ~s\"\"\"
...>3   4
...>4   3
...>2   5
...>1   3
...>3   9
...>3   3
...>\"\"\"
...> Aoc2024.Day1.part1(input)
11
"""
  def part1(input) do
    input
    |> prepare_input()
    |> parse_input()
    |> map_tuple(&Enum.sort/1)
    |> fuse_tuple()
    |> Enum.reduce(0, fn {first, second}, acc -> Kernel.abs(first - second) + acc end)
  end

@doc """
##Part 2
iex> input = ~s\"\"\"
...>3   4
...>4   3
...>2   5
...>1   3
...>3   9
...>3   3
...>\"\"\"
...> Aoc2024.Day1.part2(input)
31
"""
  def part2(input) do
    {first, second} =input
    |> prepare_input()
    |> parse_input()
      
    Enum.reduce(first, 0, fn item, acc ->  acc + Enum.count(second, fn i -> item == i end) * item  end)
  end

  defp prepare_input(input) do
    input
    |> String.trim()
    |> String.split("\n")    
  end

  defp parse_input(input) do
    input
    |> Enum.map(fn line ->
      Regex.run(~r/(\d+)\s+(\d+)/, String.trim(line))
      |> Kernel.tl()
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.reduce([], fn array_tuple , acc ->
      [{ List.first(array_tuple), List.last(array_tuple)} | acc]
    end)
    |> Enum.unzip()
  end

  def fuse_tuple({first, second}) do
    Enum.zip([first, second])
  end

  defp map_tuple({first, second}, fun) do
    {fun.(first), fun.(second)}
  end
end
