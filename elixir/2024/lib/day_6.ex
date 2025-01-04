defmodule Aoc2024.Day6 do
  
  @doc """
  ## Part1
  iex> input = ~s\"\"\"
  ...>....#.....
  ...>.........#
  ...>..........
  ...>..#.......
  ...>.......#..
  ...>..........
  ...>.#..^.....
  ...>........#.
  ...>#.........
  ...>......#...
  ...>\"\"\"
  ...> Aoc2024.Day6.part1(input)
  41
  """
 def part1(input) do
    lines = String.split(input, "\n")
    start = find_start(lines)
    grid = parse_grid(lines)

    visited = trace_path(grid, start, :n, MapSet.new())

    MapSet.size(visited)
 end

  @doc """
  ## Part2
  iex> input = ~s\"\"\"
  ...>....#.....
  ...>.........#
  ...>..........
  ...>..#.......
  ...>.......#..
  ...>..........
  ...>.#..^.....
  ...>........#.
  ...>#.........
  ...>......#...
  ...>\"\"\"
  ...> Aoc2024.Day6.part2(input)
  6
  """  
 def part2(input) do
   lines = String.split(String.trim(input), "\n")

   start = find_start(lines)
   grid = parse_grid(lines)

    visited = trace_path(grid, start, :n, MapSet.new())
    targets = MapSet.delete(visited, start)

    {row_blocks, col_blocks} = get_block_maps(lines)

    Task.async_stream(Enum.chunk_every(targets, 1000), fn targets_chunk ->
      targets_chunk
      |> Enum.count(fn {target_row, target_col} ->
        new_row_blocks = update_block_map(row_blocks, target_row, target_col)
        new_col_blocks = update_block_map(col_blocks, target_col, target_row)
        blocks = %{rows: new_row_blocks, cols: new_col_blocks}
        is_loop(blocks, start, :n, MapSet.new())
      end)
    end)
    |> Enum.map(fn {:ok, result} -> result end)
    |> Enum.sum()
 end 

  # nil means we've left the grid, so no loop
  defp is_loop(_, {nil, _}, _, _), do: false
  defp is_loop(_, {_, nil}, _, _), do: false

  defp is_loop(blocks, position, direction, turns) do
    # check the set of previous turns to see if we've entered a loop
    case MapSet.member?(turns, {position, direction}) do
      false ->
        new_turns = MapSet.put(turns, {position, direction})
        next_position = next_turn(blocks, position, direction)
        is_loop(blocks, next_position, next_dir(direction), new_turns)

      true ->
        true
    end
 end


  defp next_turn(blocks, {i, j}, :n), do: {add_one(get_lower(blocks.cols, j, i)), j}
  defp next_turn(blocks, {i, j}, :s), do: {sub_one(get_upper(blocks.cols, j, i)), j}
  defp next_turn(blocks, {i, j}, :w), do: {i, add_one(get_lower(blocks.rows, i, j))}
  defp next_turn(blocks, {i, j}, :e), do: {i, sub_one(get_upper(blocks.rows, i, j))}

  defp get_upper(block_map, ia, ib),
    do: Map.get(block_map, ia, []) |> Enum.find(&(&1 > ib))

  defp get_lower(block_map, ia, ib),
    do: Map.get(block_map, ia, []) |> Enum.reverse() |> Enum.find(&(&1 < ib))

  defp add_one(nil), do: nil
  defp add_one(v), do: v + 1

  defp sub_one(nil), do: nil
  defp sub_one(v), do: v - 1

  defp get_block_maps(lines) do
    block_positions =
      lines
      |> Enum.with_index()
      |> Enum.map(fn {line, i} ->
        line
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.filter(fn {c, _} -> c == "#" end)
        |> Enum.map(fn {_, j} -> {i, j} end)
      end)
      |> List.flatten()

    row_blocks = Enum.group_by(block_positions, fn {i, _} -> i end, fn {_, j} -> j end)
    col_blocks = Enum.group_by(block_positions, fn {_, j} -> j end, fn {i, _} -> i end)
    {row_blocks, col_blocks}
  end

  defp update_block_map(map, ia, ib),
    do: Map.update(map, ia, [ib], fn l -> Enum.sort(l ++ [ib]) end)
  
 def trace_path(grid, position, direction, visited) do
    new_visited = MapSet.put(visited, position)
    next_position = next_pos(position, direction)
    next_cell = Map.get(grid, next_position, :exit)

    case next_cell do
      :empty -> trace_path(grid, next_position, direction, new_visited)
      :block -> trace_path(grid, position, next_dir(direction), new_visited)
      :exit -> new_visited
    end
  end

  def next_pos({i, j}, :n), do: {i - 1, j}
  def next_pos({i, j}, :e), do: {i, j + 1}
  def next_pos({i, j}, :s), do: {i + 1, j}
  def next_pos({i, j}, :w), do: {i, j - 1}

  def next_dir(:n), do: :e
  def next_dir(:e), do: :s
  def next_dir(:s), do: :w
  def next_dir(:w), do: :n

  def find_start(lines) do
    {line, row} =
      lines
      |> Enum.with_index()
      |> Enum.find(fn {line, _} -> String.contains?(line, "^") end)

    {_, col} =
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.find(fn {ch, _} -> ch == "^" end)

    {row, col}
  end

  def parse_grid(lines) do
    lines
    |> Enum.with_index()
    |> Enum.map(fn {line, i} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {c, j} -> {{i, j}, get_cell_type(c)} end)
    end)
    |> List.flatten()
    |> Enum.into(%{})
  end

  defp get_cell_type("."), do: :empty
  defp get_cell_type("^"), do: :empty
  defp get_cell_type("#"), do: :block
end
