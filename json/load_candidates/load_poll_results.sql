
delete from election_raw.poll_results;

copy election_raw.poll_results from
	'/Users/bwbecker/byron/activism/pr/electionSim/json/load_candidates/2006/2006.csv'
	(format csv, encoding 'ISO-8859_5');
