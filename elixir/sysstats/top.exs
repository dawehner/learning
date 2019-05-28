alias Porcelain.Process, as: Proc

opts = [out: :stream]
proc = %Proc{out: outstream} = Porcelain.spawn("top", [], opts)

#outstream do
#  {^pid} -> IO.inspect(pid)
#end

Enum.into(outstream, IO.stream(:stdio, :line))

Proc.alive?(proc)
