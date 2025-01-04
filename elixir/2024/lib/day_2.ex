defmodule Aoc2024.Day2 do
  @doc """
  ##Part 1
  iex> input = ~s\"\"\"
  ...>7 6 4 2 1
  ...>1 2 7 8 9
  ...>9 7 6 2 1
  ...>1 3 2 4 5
  ...>8 6 4 4 1
  ...>1 3 6 7 9
  ...>\"\"\"
  ...> Aoc2024.Day2.part1(input)
  2
  """
  def part1(input) do
    input
    |> prepare_input()
    |> parse_input()
    |> Enum.map(&sliding_window(&1, fn x, y -> x - y end))
    |> Enum.filter(fn line ->
      all_increasing_or_all_decreasing?(line) && Enum.all?(line, &is_valid_level?/1)
    end)
    |> Enum.count()
  end

  defp is_valid_level?(x) do
    case Kernel.abs(x) do
      1 -> true
      2 -> true
      3 -> true
      _ -> false
    end
  end

  defp sliding_window([_] = _enum, _fun), do: []

  defp sliding_window([x, y | tail] = enum, fun) when is_list(enum) and is_function(fun) do
    [fun.(x, y) | sliding_window([y | tail], fun)]
  end

  @doc """
  ##Part 2
  iex> input = ~s\"\"\"
  ...>7 6 4 2 1
  ...>1 2 7 8 9
  ...>9 7 6 2 1
  ...>1 3 2 4 5
  ...>8 6 4 4 1
  ...>1 3 6 7 9
  ...>\"\"\"
  ...> Aoc2024.Day2.part2(input)
  4
  """
  def part2(input) do
    input
    |> prepare_input()
    |> parse_input()
    |> Enum.reduce([], fn report, acc ->
      case check_report(report) do
        {:valid, _} -> [report | acc]
        {:invalid, _} -> acc
      end
    end)
    |> Enum.count()
  end

  defp check_report(report, dampened_level_index \\ -1) do
    line =
      case dampened_level_index do
        -1 -> report
        _ -> List.delete_at(report, dampened_level_index)
      end
      |> sliding_window(fn x, y -> x - y end)

    cond do
      all_increasing_or_all_decreasing?(line) && Enum.all?(line, &is_valid_level?/1) ->
        {:valid, report}

      dampened_level_index != Enum.count(report) - 1 ->
        check_report(report, dampened_level_index + 1)

      dampened_level_index == Enum.count(report) - 1 || true ->
        {:invalid, report}
    end
  end

  defp all_increasing?(report) do
    report
    |> Enum.all?(fn x -> x > 0 end)
  end

  defp all_decreasing?(report) do
    report
    |> Enum.all?(fn x -> x < 0 end)
  end

  defp all_increasing_or_all_decreasing?(report) do
    all_increasing?(report) || all_decreasing?(report)
  end

  defp prepare_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
  end

  defp parse_input(input) do
    input
    |> Enum.map(fn line ->
      line
      |> String.split(" ")
      |> Enum.map(&String.to_integer/1)
    end)
  end
end
