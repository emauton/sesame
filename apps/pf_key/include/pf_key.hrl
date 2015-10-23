%%% @doc Shared constant and record definitions for {@link //pf_key}.
%%% Calling code may need to reference these directly before encoding or after
%%% decoding messages via a PF_KEY port process.
%%%
%%% Restricted to the subset of definitions used by {@link //sesame}.

%% Following http://tools.ietf.org/html/rfc2367
%% See that document for full descriptions of the meanings and values of
%% {@link //pf_key} records.
-define(PF_KEY_V2, 2).

%% Message types.
-define(SADB_RESERVED,   0).
-define(SADB_GETSPI,     1).
-define(SADB_UPDATE,     2).
-define(SADB_ADD,        3).
-define(SADB_DELETE,     4).
-define(SADB_GET,        5).
-define(SADB_ACQUIRE,    6).
-define(SADB_REGISTER,   7).
-define(SADB_EXPIRE,     8).
-define(SADB_FLUSH,      9).
-define(SADB_DUMP,      10).
-define(SADB_X_PROMISC, 11).
-define(SADB_X_PCHANGE, 12).
-define(SADB_MAX,       12).

%% Extension types.
-define(SADB_EXT_RESERVED,         0).
-define(SADB_EXT_SA,               1).
-define(SADB_EXT_LIFETIME_CURRENT, 2).
-define(SADB_EXT_LIFETIME_HARD,    3).
-define(SADB_EXT_LIFETIME_SOFT,    4).
-define(SADB_EXT_ADDRESS_SRC,      5).
-define(SADB_EXT_ADDRESS_DST,      6).
-define(SADB_EXT_ADDRESS_PROXY,    7).
-define(SADB_EXT_KEY_AUTH,         8).
-define(SADB_EXT_KEY_ENCRYPT,      9).

%% Security association types.
-define(SADB_SATYPE_UNSPEC, 0).
-define(SADB_SATYPE_ESP,    3).

%% Security association states.
-define(SADB_SASTATE_LARVAL, 0).
-define(SADB_SASTATE_MATURE, 1).
-define(SADB_SASTATE_DYING,  2).
-define(SADB_SASTATE_DEAD,   3).
-define(SADB_SASTATE_MAX,    3).

%% Message header.
-record(sadb_msg,
        {version     :: non_neg_integer(),    % uint8_t
         type        :: non_neg_integer(),    % uint8_t
         errno       :: non_neg_integer(),    % uint8_t
         satype      :: non_neg_integer(),    % uint8_t
         len         :: non_neg_integer(),    % uint16_t
         reserved    :: non_neg_integer(),    % uint16_t
         seq         :: non_neg_integer(),    % uint32_t
         pid         :: non_neg_integer()}).  % uint32_t

%% Security association extension.
-record(sadb_sa,
        {len         :: non_neg_integer(),    % uint16_t
         exttype     :: non_neg_integer(),    % uint16_t
         spi         :: non_neg_integer(),    % uint32_t
         replay      :: non_neg_integer(),    % uint8_t
         state       :: non_neg_integer(),    % uint8_t
         auth        :: non_neg_integer(),    % uint8_t
         encrypt     :: non_neg_integer(),    % uint8_t
         flags       :: non_neg_integer()}).  % uint32_t

%% Lifetime extension.
-record(sadb_lifetime,
        {len         :: non_neg_integer(),    % uint16_t
         exttype     :: non_neg_integer(),    % uint16_t
         allocations :: non_neg_integer(),    % uint32_t
         bytes       :: non_neg_integer(),    % uint64_t
         addtime     :: non_neg_integer(),    % uint64_t
         usetime     :: non_neg_integer()}).  % uint64_t

%% Address extension.
-record(sadb_address,
        {len         :: non_neg_integer(),    % uint16_t
         exttype     :: non_neg_integer(),    % uint16_t
         proto       :: non_neg_integer(),    % uint8_t
         prefixlen   :: non_neg_integer(),    % uint8_t
         reserved    :: non_neg_integer()}).  % uint16_t

%% Key extension.
-record(sadb_key,
        {len         :: non_neg_integer(),    % uint16_t
         exttype     :: non_neg_integer(),    % uint16_t
         bits        :: non_neg_integer(),    % uint16_t
         reserved    :: non_neg_integer()}).  % uint16_t
