defmodule Utils do
  @moduledoc """
  Utils module is a module with some common utility functions reusable across different
  places
  """

  import Req

  @default_year 2024
  @session_token System.get_env("AOC_AUTH_TOKEN")
  @puzzle_path System.get_env("AOC_PUZZLE_PATH")

  @doc """
  Returns the input for the given day of the year 2024
  """
  def get_input(day, year \\ @default_year) when is_number(day) and is_number(year) do
    file_path = @puzzle_path <> "/2024/#{day}.txt"

    unless File.exists?(file_path) do
      day
      |> fetch_input(year)
      |> save_input(file_path)
    end

    File.read!(file_path)
  end

  @doc """
  Get the given input for a given day from adventofcode.com 
  """
  defp fetch_input(day, year) when is_number(day) and is_number(year) do
    req =
      Req.new(
        base_url: "https://adventofcode.com",
        headers: %{"cookie" => "session=#{@session_token}"}
      )

    Req.get!(req, url: "/#{year}/day/#{day}/input").body
  end

  @doc """
  Save a puzzle into a file that will be stored in the path defined in AOC_PUZZLE_PATH
  """
  defp save_input(content, file_path) when is_bitstring(content) do
    File.write!(file_path, content)
  end
end
