
with	
candidates as (
	select riding_id, candidate_name_last || ', ' || candidate_name_first as candidate_name,
			party_id, incumbent, elected, votes
	from	election_raw.poll_results
			join election.party using (party_name)
),

cand_sum as (
	select	distinct on (riding_id, candidate_name) riding_id, candidate_name, party_id, incumbent, elected, 
			sum(votes) over (partition by riding_id, candidate_name) as votes
	from	candidates
)


select	json_agg(json_build_object(
      	'riding_id', riding_id, 
      	'candidate_name', candidate_name,
      	'party_id', party_id, 
      	'incumbent', incumbent, 
      	'elected', elected,
      	'votes', votes 
 	)) 
from cand_sum
 	