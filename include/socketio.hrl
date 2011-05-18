-record(msg,
	{
    	  content = []     :: string() | jsx:eep0018(),
 	  json    = false  :: boolean(),
	  length  = 0      :: non_neg_integer()
	}).

-record(heartbeat,
	{
	  index :: non_neg_integer()
	 }).
