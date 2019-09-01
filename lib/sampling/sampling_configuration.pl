:-module(sampling_configuration, [k_random_sampling/1
				 ]).

/** <module> Configuration options for sampling library.
*/


%!	k_random_sampling(?Sampling) is det.
%
%	The type of Sampling to use in k_list_samples/3.
%
%	The following Sampling types are known:
%	* randset: Uses randset/3 to select sample indices at random.
%	* knuth: Knuth's sampling algorithm S.
%
%	There are tradeoffs between "knuth" and "randset". For instance,
%	"randset" doesn't need to walk to the end of a list, but knuth's
%	might be faster when the sample size is small, etc.
%
k_random_sampling(randset).
%k_random_sampling(knuth).
