defmodule Solution do
  defmodule Walk do
    defstruct path: [], fullpath: [], nodes: %{}, cur: "", steps: 0
  end

  def run() do
    {path, nodes} = parse(readlines())

    # Part one
    walk(path, nodes, "AAA") |> IO.puts

    # Part two
    Map.keys(nodes)
    |> Enum.filter(&String.ends_with?(&1, "A"))
    |> Enum.map(&walk(path, nodes, &1))
    |> Enum.reduce(&lcm/2)
    |> IO.puts
  end

  defp walk(path, nodes, cur), do: walk(%Walk{path: path, fullpath: path, nodes: nodes, cur: cur})
  defp walk(s = %Walk{cur: <<_::size(16), "Z">>}), do: s.steps
  defp walk(s = %Walk{path: []}), do: walk(%{s|path: s.fullpath})
  defp walk(s = %Walk{path: [p|path]}) do
    next = s.nodes[s.cur] |> elem(p)
    walk(%{s|path: path, cur: next, steps: s.steps + 1})
  end

  defp parse(lines), do: parse(lines, :path, "", %{})
  defp parse([line|lines], :path, _, nodes) do
    path =
      line
      |> String.graphemes
      |> Enum.map(&(if &1 == "L", do: 0, else: 1))
    parse(lines, :nodes, path, nodes)
  end
  defp parse([], :nodes, path, nodes), do: {path, nodes}
  defp parse([""|lines], :nodes, path, nodes), do: parse(lines, :nodes, path, nodes)
  defp parse([line|lines], :nodes, path, nodes) do
    [node, turns] =
      line
      |> String.trim
      |> String.split(" = ")
    [left, right] =
      turns
      |> String.trim_leading("(")
      |> String.trim_trailing(")")
      |> String.split(", ")
    nodes =
      Map.put_new(nodes, node, {left, right})
    parse(lines, :nodes, path, nodes)
  end

  defp readlines(lines \\ []) do
    case IO.read(:stdio, :line) do
      :eof -> Enum.reverse(lines)
      line -> readlines([String.trim(line)|lines])
    end
  end

  defp lcm(a, b), do: div(a, gcd(a, b)) * b

  defp gcd(a, 0), do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))
end

Solution.run
