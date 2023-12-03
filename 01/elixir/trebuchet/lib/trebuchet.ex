defmodule Trebuchet do
  @moduledoc """
  Documentation for `Trebuchet`.
  """

  @doc """
  Trebuchet

  ## Examples

      iex> Trebuchet.run(Path.join([File.cwd!,"../../fixture.txt"]))
      142

  """
  def run(file_path) do
    file_path
    |> File.read!
    |> String.split("\n", trim: true)
    |> Enum.map(&String.replace(&1, ~r/[a-z]+/, ""))
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(&to_number/1)
    |> Enum.reduce(0, fn(acc, item) -> acc + item end)
  end

  def to_number(_list = [single_digit | _tail = []]) do
   String.to_integer(single_digit <> single_digit)
  end

  def to_number(_list = [head | _tail = [single_digit]]) do
    String.to_integer(head <> single_digit)
  end
    
  def to_number(_list = [head | tail]) do
    String.to_integer(head <> List.last(tail))
  end
  
end
