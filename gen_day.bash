mkdir "./src/Day${1}"
sed "s/DAY_NUM/${1}/g" ./DayX.purs > "./src/Day${1}/Day${1}.purs"
touch "./src/Day${1}/sample.txt"
touch "./src/Day${1}/real.txt"