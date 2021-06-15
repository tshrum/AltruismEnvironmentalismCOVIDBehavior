

first_switch = function(obs) {
  switch_point = 0
  for (i in 1:length(obs)) {
    if (obs[i]==2) {
      switch_point = i
      break
    }
  }
  return(switch_point) 
}

count_na = function(obs) {
  na_count = 0
  for (i in 1:length(obs)) {
    if (obs[i]==0) {
      na_count = na_count + 1
    }
  }
  return(na_count)
}

count_switches = function(obs) {
  switches = 0
  for (i in 2:length(obs)) {
    if (obs[i-1] != obs[i]) {
      switches = switches + 1
    }
  }
  return(switches)
}

