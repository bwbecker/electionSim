
delete from election_raw.poll_results;

copy election_raw.poll_results from
	'/Users/bwbecker/byron/activism/pr/electionSim/json/2011/2011_results_clean.csv'
	(format csv, encoding 'ISO-8859_5');
