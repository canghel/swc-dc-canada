declare -a LOCATIONS=("us.png" "ca.png" "gb.png" "au.png")

for COUNTRY in "${LOCATIONS[@]}"; do
	for YEAR in {2012..2018}; do
		echo $COUNTRY 
		echo $YEAR
		TIMES=($(grep -A1 "$COUNTRY" ../data/2018-04-17-swc-list.html  | grep "$YEAR" | wc -l))
		#printf  "%-30s  %-30s  %-30s" "$COUNTRY" "$YEAR" "$TIMES" >> countryCounts.txt
		printf '%s\n' "$COUNTRY" "$YEAR" "$TIMES" | paste -sd ',' >> ../data/2018-04-20-countryCounts.txt
	done
done