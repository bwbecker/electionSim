
# Getting Data

* It's hard to find the raw data all in one file.  Find the 2011 data at
  http://www.elections.ca/content.aspx?section=res&dir=rep/off/41gedata&document=bypro&lang=e
  "Poll results" is easiest to work with.  Downloads a zip file containing one CSV for each
  of the 308 ridings.

* Edit load_poll_results.sql to adjust file name.

* cat pollresults* | grep '^[0-9]' > 2011_results.csv
* psql service=election < load_poll_results.sql
* psqlJson election < extract_308_candidates.sql > candidates.json


* Extract ridings:
	Derived from the candidates file joined with population data from
   	http://www.elections.ca/content.aspx?section=res&dir=cir/list&document=index&lang=e#list
   	The pop data was cleaned up manually in a text document
   	* copy into table riding_pop with a query such as 
   		copy riding_pop from '/Users/bwbecker/byron/activism/pr/electionSim/json/load_candidates/2006_pop_by_riding.txt'

	* psqlJson election < extract_308_ridings.sql > ridings.json
	