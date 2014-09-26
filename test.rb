sample :drum_tom_mid_soft
sleep 1
sample :drum_tom_mid_hard

in_thread(name: :example) do
  sample :ambi_choir, rate: 0.2
end
