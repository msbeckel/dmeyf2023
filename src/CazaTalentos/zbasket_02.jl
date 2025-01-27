using Random



Random.seed!(594697)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end


# defino las jugadoras
mejor = [0.7]
peloton = Vector((501:599) / 1000)
jugadoras = append!(mejor, peloton)

# veo que tiene el vector
jugadoras

# hago que las 100 jugadoras tiren 10 veces cada una
res = ftirar.(jugadoras, 10)

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 10)  # 10 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)

  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)
