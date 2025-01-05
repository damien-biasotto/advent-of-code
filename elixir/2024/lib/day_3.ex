defmodule Aoc2024.Day3 do
  defmodule Parser do
    import NimbleParsec

    multiply =
      ignore(string("mul"))
      |> ignore(string("("))
      |> integer(min: 1)
      |> ignore(string(","))
      |> integer(min: 1)
      |> ignore(string(")"))
      |> tag(:multiply)
    

    defparsec(:multiply, eventually(multiply) |> repeat())
  end

  @doc """
  ##Part 1
  iex> input = ~s\"\"\"
  ...>xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
  ...>\"\"\"
  ...> Aoc2024.Day3.part1(input)
  161
  """
  def part1(input) do
    input
    |> prepare_input()
    |> parse_input()
  end

  @doc """
  ##Part 2
  iex> input = ~s\"\"\"
  ...>xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
  ...>\"\"\"
  ...> Aoc2024.Day3.part2(input)
  48
  """
  def part2(input) do
    input
    |> prepare_input()
    |> parse_input()
  end

  defp prepare_input(input) do
    input
    |> String.trim()
  end

  defp parse_input(input) do
    {:ok, data, _, _, _, _} =
      input
      |> Parser.multiply()

    data |> Enum.reduce(0, fn {:multiply, prods}, acc -> Enum.product(prods) + acc end)
  end
end
B
