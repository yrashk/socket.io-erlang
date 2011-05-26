-record(msg,
	{
    content = []     :: string() | jsx:eep0018(),
 	  json    = false  :: boolean()
	}).

-record(heartbeat,
	{
	  index :: non_neg_integer()
	 }).
