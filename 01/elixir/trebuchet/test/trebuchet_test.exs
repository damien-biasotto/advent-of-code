defmodule TrebuchetTest do
  use ExUnit.Case
  doctest Trebuchet

  test "Checking the code against the test data set" do
    fixture_path = Path.join([File.cwd!, "..", "..", "fixture.txt"])
    assert Trebuchet.run(fixture_path) == 142
  end

  test "Checking the code against the real data set" do
    fixture_path = Path.join([File.cwd!, "..", "..", "input.txt"])
    assert Trebuchet.run(fixture_path) == 55538
  end
 
end
