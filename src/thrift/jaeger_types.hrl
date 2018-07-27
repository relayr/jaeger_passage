-ifndef(_jaeger_types_included).
-define(_jaeger_types_included, yeah).

-define(JAEGER_TAGTYPE_STRING, 0).
-define(JAEGER_TAGTYPE_DOUBLE, 1).
-define(JAEGER_TAGTYPE_BOOL, 2).
-define(JAEGER_TAGTYPE_LONG, 3).
-define(JAEGER_TAGTYPE_BINARY, 4).

-define(JAEGER_SPANREFTYPE_CHILD_OF, 0).
-define(JAEGER_SPANREFTYPE_FOLLOWS_FROM, 1).

%% struct 'Tag'

-record('Tag', {'key' :: string() | binary(),
                'vType' :: integer(),
                'vStr' :: string() | binary() | undefined,
                'vDouble' :: float() | undefined,
                'vBool' :: boolean() | undefined,
                'vLong' :: integer() | undefined,
                'vBinary' :: string() | binary() | undefined}).
-type 'Tag'() :: #'Tag'{}.

%% struct 'Log'

-record('Log', {'timestamp' :: integer(),
                'fields' = [] :: list()}).
-type 'Log'() :: #'Log'{}.

%% struct 'SpanRef'

-record('SpanRef', {'refType' :: integer(),
                    'traceIdLow' :: integer(),
                    'traceIdHigh' :: integer(),
                    'spanId' :: integer()}).
-type 'SpanRef'() :: #'SpanRef'{}.

%% struct 'Span'

-record('Span', {'traceIdLow' :: integer(),
                 'traceIdHigh' :: integer(),
                 'spanId' :: integer(),
                 'parentSpanId' :: integer(),
                 'operationName' :: string() | binary(),
                 'references' :: list() | undefined,
                 'flags' :: integer(),
                 'startTime' :: integer(),
                 'duration' :: integer(),
                 'tags' :: list() | undefined,
                 'logs' :: list() | undefined}).
-type 'Span'() :: #'Span'{}.

%% struct 'Process'

-record('Process', {'serviceName' :: string() | binary(),
                    'tags' :: list() | undefined}).
-type 'Process'() :: #'Process'{}.

%% struct 'Batch'

-record('Batch', {'process' = #'Process'{} :: 'Process'(),
                  'spans' = [] :: list()}).
-type 'Batch'() :: #'Batch'{}.

%% struct 'BatchSubmitResponse'

-record('BatchSubmitResponse', {'ok' :: boolean()}).
-type 'BatchSubmitResponse'() :: #'BatchSubmitResponse'{}.

-endif.
