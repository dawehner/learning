#current_process = self()
#
#spawn_link(fn ->
#  send current_process, {:msg 0}
#end)
#
#receive do
#  {:msg, contents} -> IO.puts contents
#end

defmodule MarkovChain do
  def nextElement(current_markup_element) do
    markov_chain = [
      [0, 0.5, 0.5],
      [0.5, 0, 0.5],
      [0.5, 0.5, 0.0]
    ]

    probability_row_chain = convertMarkovRowToProbality(Enum.at(markov_chain, current_markup_element), 0.0)
    randomNumber = :rand.uniform
    IO.puts(randomNumber)

    selectNextElement(probability_row_chain, randomNumber)
  end

  def convertMarkovRowToProbality([head | tail], probabilitySum) do
    currentProbablity = probabilitySum + head
    [currentProbablity | convertMarkovRowToProbality(tail, currentProbablity)]
  end

  def convertMarkovRowToProbality([], _) do
    []
  end

  # [0, 0.5, 1.0] 0.6 => 2, 0.4 => 1
  def selectNextElement(list, probablity) do
    Enum.find_index(list, &(&1 > probablity))
  end

end

IO.puts MarkovChain.nextElement(0)
