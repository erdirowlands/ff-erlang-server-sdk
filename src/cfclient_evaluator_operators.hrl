%% Group Rule Association Operator - in binary format as we use it to match on binary data from server (which is also camel case)
-define(SEGMENT_MATCH_OPERATOR, <<"segmentMatch">>).
%% Custom Rule Operators
-define(EQUAL_OPERATOR, equal).
-define(EQUAL_SENSITIVE_OPERATOR, equal_sensitive).
-define(STARTS_WITH_OPERATOR, starts_with).
-define(ENDS_WITH_OPERATOR, ends_with).
-define(CONTAINS_OPERATOR, contains).
-define(IN_OPERATOR, in).
